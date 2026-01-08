# Set NASA Earthdata Credentials

Set NASA Earthdata credentials for use in `chewie` functions.

## Usage

``` r
chewie_creds(
  netrc = NULL,
  path = NULL,
  force = FALSE,
  usr = NULL,
  pwd = NULL,
  quiet = FALSE,
  test = TRUE,
  renviron = "auto"
)

chewie_clean_netrc(renviron = "auto")

chewie_set_netrc_env(netrc, renviron = "auto")

chewie_get_netrc()
```

## Arguments

- netrc:

  character path to an existing `.netrc` file.

- path:

  character path to set where to create the `.netrc` file

- force:

  logical whether to overwrite an existing `.netrc` or `CHEWIE_NETRC`
  environment variable.

- usr:

  character NASA Earthdata username.

- pwd:

  character NASA Earthdata password.

- quiet:

  logical whether to suppress the registration prompt and messaging.

- test:

  logical whether to test the credentials after setting them.

- renviron:

  character either "auto" "user", '"project"' or path to the directory
  containing the `.Renviron` file to set the `CHEWIE_NETRC` environment.

## Details

The NASA Earthdata API requires a username and password to access data.
This is provided via the config settings in the form of a `.netrc` file.
The `chewie_creds` function can help to generate this file and set its
location as an environment variable for use in `chewie` functions. In
theory this should only need to be done once, but if you need to change
your credentials you can use the `force` argument to overwrite the
existing `.netrc` file.

`chewie_env_clean` can be used to manually remove the `CHEWIE_NETRC`
environment and delete the associated `.netrc` file.

`chewie_set_netrc_env` is most likely not required but can be used to
manually set the `CHEWIE_NETRC` environment variable which is used for
authenticating downloads from the NASA Earthdata API.

`chewie_get_netrc` can be used to manually get the `CHEWIE_NETRC`
environment, providing the file path to the `.netrc` file used to
authenticate any requests to the NASA Earthdata API
