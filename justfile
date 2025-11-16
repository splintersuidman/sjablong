default:
    just --list

# build the Spago project
[group("build")]
build:
    spago build

# generate .tidyoperators file for formatting
[group("format")]
_format-generate-operators:
	spago sources | xargs purs-tidy generate-operators > .tidyoperators

# format PureScript source
[group("format")]
format: _format-generate-operators
	purs-tidy format-in-place "src/**/*.purs"

# check if formatting is needed
[group("format")]
format-check: _format-generate-operators
	purs-tidy check "src/**/*.purs"
