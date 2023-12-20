load("@//:deps.bzl", "STACKAGE_DEPS", "aoc")

load("@rules_haskell//haskell:defs.bzl", "haskell_binary", "haskell_repl", "haskell_library")

haskell_binary(
    name = "main",
    srcs = ["Main.hs"],
    deps = STACKAGE_DEPS,
)

haskell_repl(
    name = "repl",
    collect_data = False,
    deps = STACKAGE_DEPS,
)

load("@rules_rust//rust:defs.bzl", "rust_binary")

rust_binary(
    name = "main-rs",
    srcs = ["main.rs"],
)

haskell_library(
    name = "aoclib",
    srcs = ["AocLib.hs"],
    deps = STACKAGE_DEPS,
)

[aoc(day + 1) for day in range(2)]