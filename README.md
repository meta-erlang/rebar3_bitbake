# rebar3_bitbake

Generate BitBake recipes utilizing the classes from [meta-erlang](https://github.com/joaohf/meta-erlang) Yocto Project/OE layer.


## Use

Add the plugin as `project_plugins` to your rebar config:

    {project_plugins, [
        {rebar3_bitbake, {git, "https://github.com/meta-erlang/rebar3_bitbake.git", {tag, "0.1.0"}}}
    ]}.

Then just call the bitbake plugin directly in an existing application:


    $ rebar3 bitbake
    ===> Fetching rebar3_bitbake
    ===> Compiling rebar3_bitbake
    <Plugin Output>

Important: make sure that the existing application has release configuration. The rebar3_bitbake plugin checks if a release can be found and will refuse to create a bitbake recipe if there is no such release. The rebar3.bbclass from meta-erlang works based on [rebar3 release configuration](http://rebar3.org/docs/deployment/releases/).


## License

[Apache 2 License](https://github.com/joaohf/edocmermaid/LICENSE)