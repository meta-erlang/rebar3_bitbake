%% @doc Generates a bitbake recipe from Erlang application.
%%
%% Basically what this provide does is walking through all the steps
%% in order to create a bitbake recipe from the current application using
%% data provided by rebar3.
%%
%% @reference See <a href="https://docs.yoctoproject.org/dev-manual/dev-manual-common-tasks.html#writing-a-new-recipe">Writing a New Recipe</a>
%% for the complete steps.
%%
%% @end
-module(rebar3_bitbake_prv).

-export([init/1, do/1, format_error/1]).

-define(REBAR3_BITBAKE_VER, "0.1.0").
-define(PROVIDER, bitbake).
-define(DEPS, [app_discovery]).

-define(DEFAULT_LICENSE_FILES, ["LICENSE", "LICENSE.md", "COPYING", "COPYING.md"]).
-define(LICENSE_CLOSED, "CLOSED").

-type digest_hex() :: string().
-type uri() :: string().
-type git_ref() :: string().

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 bitbake"},
        % list of options understood by the plugin
        {opts, []},
        {short_desc, "Generate BitBake recipes utilizing the classes from meta-erlang"},
        {desc, "Generate BitBake recipes utilizing the classes from meta-erlang"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    check_release_presence(State),
    %% search for this plugin's appinfo in order to know
    %% where to look for the mustache templates
    Apps = rebar_state:all_plugin_deps(State),
    PluginInfo = plugin_appinfo(Apps),
    PluginDir = rebar_app_info:dir(PluginInfo),
    Tpl = load_template(PluginDir),
    [AppInfo | _] = rebar_state:project_apps(State),
    Name = rebar_app_info:name(AppInfo),
    Vsn = vsn(AppInfo),
    Dir = rebar_state:dir(State),
    RecipeFilename = make_recipe_filename(Name, Vsn),
    RecipeFile = filename:join([Dir, RecipeFilename]),
    Vars = make_variables(State, AppInfo),

    rebar_api:debug("bitbake variables: ~p\n", [Vars]),

    write_file(RecipeFile, render(Tpl, Vars)),
    rebar_api:info("Wrote: ~s", [RecipeFilename]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

plugin_appinfo(Apps) ->
    plugin_appinfo(Apps, undefined).

plugin_appinfo([], AppInfo) ->
    AppInfo;
plugin_appinfo([AppInfo | Rest], _) ->
    case rebar_app_info:name(AppInfo) of
        <<"rebar3_bitbake">> ->
            plugin_appinfo([], AppInfo);
        _ ->
            plugin_appinfo(Rest, undefined)
    end.

-spec write_file(Output, Data) -> ok | no_return() when Output :: file:name_all(), Data :: iodata().
write_file(Output, Data) ->
    ok = filelib:ensure_dir(Output),
    case file:write_file(Output, Data) of
        ok -> ok;
        {error, Reason} -> rebar_api:abort("Failed to write output file ~p: ~p\n", [Output, Reason])
    end.

-spec load_template(PluginDir) -> Bin when PluginDir :: file:name_all(), Bin :: iodata().
load_template(PluginDir) ->
    Name = filename:join([PluginDir, "priv", "bitbake.template"]),
    rebar_api:debug("template loaded ~p\n", [Name]),
    {ok, Bin} = file:read_file(Name),
    Bin.

-spec render(Bin, Context) -> Bin when Bin :: iodata(), Context :: proplists:proplist().
render(Bin, Context) ->
    bbmustache:render(
        rebar_utils:to_binary(Bin),
        Context,
        [{key_type, atom}]
    ).

-spec make_recipe_filename(Name, Vsn) -> RecipeFilename when
    Name :: string(),
    Vsn :: string(),
    RecipeFilename :: string().
make_recipe_filename(Name, Vsn) ->
    io_lib:format("~s_~s.bb", [Name, Vsn]).

-spec make_variables(State, AppInfo) -> Variables when
    State :: rebar_state:t(),
    AppInfo :: rebar_app_info:t(),
    Variables :: proplists:proplist().
make_variables(State, AppInfo) ->
    Dir = rebar_state:dir(State),
    AppDetails = rebar_app_info:app_details(AppInfo),
    License = license(AppDetails),
    [
        {rebar3_bitbake_ver, ?REBAR3_BITBAKE_VER},
        {name, rebar_app_info:name(AppInfo)},
        {summary, description(AppDetails)},
        {license, License},
        {homepage, homepage(AppDetails)},
        {project_src_uri, src_uri(Dir)},
        {project_src_rev, git_ref(Dir)},
        {lic_files, license_files(Dir, License)}
    ].

-spec vsn(AppInfo) -> Version when AppInfo :: rebar_app_info:t(), Version :: string().
vsn(AppInfo) ->
    case rebar_app_info:original_vsn(AppInfo) of
        Vsn when is_binary(Vsn) -> binary_to_list(Vsn);
        Vsn when is_list(Vsn) -> Vsn
    end.

-spec description(AppDetails) -> Description when
    AppDetails :: proplists:proplist(), Description :: string().
description(AppDetails) ->
    proplists:get_value(description, AppDetails).

-spec license(AppDetails) -> License when AppDetails :: proplists:proplist(), License :: string().
license(AppDetails) ->
    case proplists:get_value(licenses, AppDetails) of
        undefined ->
            ?LICENSE_CLOSED;
        [License | _] ->
            sanitize_license(License)
    end.

sanitize_license(License) ->
    string:replace(License, " ", "-").

-spec license_files(Dir, License) -> LicenseLicFile when
    Dir :: file:name_all(),
    License :: string(),
    LicenseLicFile :: iodata().
license_files(_Dir, ?LICENSE_CLOSED) ->
    [];
license_files(Dir, _License) ->
    LicFiles = [
        {File, get_md5(File)}
        || File <- ?DEFAULT_LICENSE_FILES, filelib:is_regular(filename:join(Dir, File))
    ],
    [io_lib:format("file://~s;md5=~s \\~n", [File, Md5]) || {File, Md5} <- LicFiles].

-spec homepage(AppDetails) -> Homepage when
    AppDetails :: proplists:proplist(), Homepage :: string().
homepage(AppDetails) ->
    case proplists:get_value(links, AppDetails) of
        [] ->
            rebar_api:warn(
                "At least one link item is needed as homepage. Considering add a new item `links` in the file app.src",
                []
            ),
            "";
        [{_Name, Link} | _Rest] ->
            Link
    end.

-spec git_ref(Dir) -> Ref when
    Dir :: file:name_all(),
    Ref :: git_ref().
git_ref(Dir) ->
    case
        rebar_utils:sh(
            "git rev-parse HEAD",
            [
                {use_stdout, false},
                return_on_error,
                {cd, Dir}
            ]
        )
    of
        {error, _} ->
            rebar_api:abort("Getting ref of git repo failed in ~ts. ", [Dir]);
        {ok, String} ->
            rebar_string:trim(String, both, "\n")
    end.

%% Regex used for parsing scp style remote url
-define(SCP_PATTERN, "\\A(?<proto>[^@]+)://(?<host>[^:]+)\\z").

-spec src_uri(Dir) -> Uri when
    Dir :: file:name_all(),
    Uri :: uri().
src_uri(Dir) ->
    ShOpts = [{cd, Dir}],

    {ok, ScpUrl} = rebar_utils:sh("git config --get remote.origin.url", ShOpts),

    Url =
        case re:run(ScpUrl, ?SCP_PATTERN, [{capture, [host], list}, unicode]) of
            {match, [Host]} ->
                rebar_string:trim(Host, both, "\n");
            nomatch ->
                rebar_api:abort("Getting url for git repo failed in ~ts. ", [Dir])
        end,

    {ok, ScpBranch} = rebar_utils:sh("git rev-parse --abbrev-ref HEAD", ShOpts),

    Branch = rebar_string:trim(ScpBranch, both, "\n"),

    "git://" ++ Url ++ ";branch=" ++ Branch.

-spec check_release_presence(State) -> Presence | no_return() when
    State :: rebar_state:t(),
    Presence :: boolean().
check_release_presence(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    case lists:keyfind(release, 1, RelxConfig) of
        {release, {Name0, _Ver}, _} ->
            atom_to_list(Name0);
        {release, {Name0, _Ver}, _, _} ->
            atom_to_list(Name0);
        false ->
            rebar_api:abort("No relx release has detected. Unable to create recipe.", [])
    end.

% from rebar_prv_local_upgrade.erl
-spec get_md5(LicenseFilePath) -> DigestHex when
    LicenseFilePath :: file:name_all(),
    DigestHex :: digest_hex().
get_md5(LicenseFilePath) ->
    {ok, LicenseFile} = file:read_file(LicenseFilePath),
    Digest = crypto:hash(md5, LicenseFile),
    DigestHex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Digest)]),
    rebar_string:lowercase(DigestHex).
