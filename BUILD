load("@//:deps.bzl", "PACKAGES")

load("@rules_haskell//haskell:defs.bzl", "haskell_binary", "haskell_repl")

STACKAGE_DEPS = ["@stackage//{}".format(it) for it in PACKAGES]

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