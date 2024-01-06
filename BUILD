load("@//:deps.bzl", "STACKAGE_DEPS", "aoc")

load("@rules_haskell//haskell:defs.bzl", "haskell_repl", "haskell_library")

haskell_repl(
    name = "repl",
    collect_data = False,
    deps = STACKAGE_DEPS,
)

haskell_library(
    name = "aoclib",
    srcs = ["AocLib.hs"],
    deps = STACKAGE_DEPS,
)

load("@rules_rust//rust:defs.bzl", "rust_library")

rust_library(
    name = "aoclib-rs",
    srcs = ["aoc.rs"],
    deps = ["@crate_index//:nom"],
)

[aoc(day + 1) for day in range(24)]

config_setting(
    name = "profiling",
    values = {
        "compilation_mode": "dbg",
    }
)
