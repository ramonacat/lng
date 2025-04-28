#!/usr/bin/env bash

set -euo pipefail
set -x

cargo fmt
pushd compiler
cargo test
popd
cargo run
cargo clippy
