<h1>Web Server to handle dbc files<h1>
<h2>The server stores received .dbc files in "dbc" folder and saves parsed results to "index" folder.<h2>
<h2>Index folder contains folders for every .dbc file with:<p>
- meta.json file with metadata such as File name, file size, handling status and timestamp<p>
- index.json with list of messages<p>
- msg_... files for every message with particular message info <h2>

<h2>Build instruction:<h2>
<p>make clean<p>
make <p>
make run<p>

<h2>Common tests instruction:<h2>
<p>make ct_clean<p>
make ct<p>
<p> To run covergae(90% now)<p>
make coverage<p>

<h2>Change log level:<h2>
in erlang console call<p>
log:set_level(<level>).<p>

Repo with dbc files:<p>
https://github.com/commaai/opendbc