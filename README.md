# Hosts

Heavily inspired by Steven Black [hosts](https://github.com/StevenBlack/hosts). This project is a host file blocklist aggregator. 

## Why?
I wanted to package Steven Black [hosts](https://github.com/StevenBlack/hosts) project to run via a cron job generating blocklists for my local [unbound](https://github.com/NLnetLabs/unbound) DNS resolver. I found the following things which I was not happy with:

 - Complex configuration mechanism instead of a single, simple, non json configuration file.
 - Hard to package as a system package for deployment
 - No [unbound](https://github.com/NLnetLabs/unbound) output format
 
 
## Features
  - Download, aggregate and deduplicate host block files
  - Output to hosts file format or unbound format
  - Single, simple configuration file
  - Easy to package as yum/deb/... package with a single static executable binary.
   
## Configuration file format
The configuration file uses the [toml](https://github.com/toml-lang/toml) format. 

Sources are defined as below. The only requirements are:
  - A unique key for the source i.e. `[[source.example]]`
  - A `url` value. The url must be a link to a file in the hosts file format.
  
All other source fields are optional and right now are for metadata only. See the [sample-config.toml](sample-config.toml) file for reference, this file is based on the configured data sources from Steven Black [hosts](https://github.com/StevenBlack/hosts) project.

```toml
[source]

  [[source.badd-boyz-hosts]]
  name = "Mitchell Krog's - Badd Boyz Hosts"
  description = "Sketchy domains and Bad Referrers from my Nginx and Apache Bad Bot and Spam Referrer Blockers"
  homeurl = "https://github.com/mitchellkrogza/Badd-Boyz-Hosts"
  issues = "https://github.com/mitchellkrogza/Badd-Boyz-Hosts/issues"
  url = "https://raw.githubusercontent.com/mitchellkrogza/Badd-Boyz-Hosts/master/hosts"
  license = "MIT"

  [[source.kad-hosts]]
  name = "KADhosts"
  description = "Fraud/adware/scam websites."
  homeurl = "https://kadantiscam.netlify.com"
  issues = "https://github.com/PolishFiltersTeam/KADhosts/issues"
  url = "https://raw.githubusercontent.com/PolishFiltersTeam/KADhosts/master/KADhosts_without_controversies.txt"
  license = "CC BY-SA 4.0"
```

## Usage

```
$ hosts --help
Host blocklist generator

Usage: hosts --conf FILE [--ip IP] [-f|--format ARG]
  Generate a lists of DNS hosts to block from configurable source databases

Available options:
  --conf FILE              Config file location
  --ip IP                  Target IP, defaults to 0.0.0.0.
  -f,--format ARG          Output format. Valid values hosts|unbound. Defaults
                           to hosts if not set
  -h,--help                Show this help text
```

```
$ hosts --conf sample-config.toml -f unbound > block.conf
```

In your unbound config `/var/unbound/etc/unbound.conf` you can add the below line in the `server` block.
```
include: /path/to/block.conf
```

### Sample unbound output
```
local-zone: "0.0.0.0.creative.hpyrdr.com" redirect
local-data: "0.0.0.0.creative.hpyrdr.com A 0.0.0.0"
local-zone: "0.0.0.0.hpyrdr.com" redirect
local-data: "0.0.0.0.hpyrdr.com A 0.0.0.0"
local-zone: "0.nextyourcontent.com" redirect
local-data: "0.nextyourcontent.com A 0.0.0.0"
local-zone: "0.r.msn.com" redirect
local-data: "0.r.msn.com A 0.0.0.0"
```

### Sample hosts output
```
0.0.0.0 0.0.0.0.creative.hpyrdr.com
0.0.0.0 0.0.0.0.hpyrdr.com
0.0.0.0 0.nextyourcontent.com
0.0.0.0 0.r.msn.com
0.0.0.0 000free.us
0.0.0.0 000tristanprod.free.fr
0.0.0.0 005.free-counter.co.uk
0.0.0.0 006.free-counter.co.uk
```

## Building from source
You will need [cabal](https://github.com/haskell/cabal) installed. If you use [nix](https://github.com/NixOS/nix) you can use the provided `nix-shell`.

When the project is cloned to your machine run:

```
cabal install
```
