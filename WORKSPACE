load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "298f6f0db23391274b4eca215daa01797d05699469048ef94540d5829b466377",
    strip_prefix = "rules_haskell-0.17",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.17.tar.gz",
)


load( "@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "9.4.7")

load("@//:deps.bzl", "PACKAGES")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    snapshot = "lts-21.5",
    packages = PACKAGES,
    stack_snapshot_json = "//:stack.json",
    components = { "attoparsec": ["lib:attoparsec", "lib:attoparsec-internal"] },
    components_dependencies = {
        # https://github.com/tweag/rules_haskell/issues/1439#issuecomment-1022055315
        "attoparsec": json.encode({"lib:attoparsec": ["lib:attoparsec-internal"]}),
    },
)

# Rust

http_archive(
    name = "rules_rust",
    sha256 = "75177226380b771be36d7efc538da842c433f14cd6c36d7660976efb53defe86",
    urls = ["https://github.com/bazelbuild/rules_rust/releases/download/0.34.1/rules_rust-v0.34.1.tar.gz"],
)


load("@rules_rust//rust:repositories.bzl", "rules_rust_dependencies", "rust_register_toolchains")

rules_rust_dependencies()

rust_register_toolchains(edition="2021", versions=["nightly/2023-12-01"])

load("@rules_rust//tools/rust_analyzer:deps.bzl", "rust_analyzer_dependencies")

rust_analyzer_dependencies()

