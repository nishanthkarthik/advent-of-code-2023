load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

load("@rules_rust//rust:defs.bzl", "rust_binary")

PACKAGES = ["array", "base", "text", "containers", "split", "parallel", "extra", "directory", "unix",
            "attoparsec", "random"]

STACKAGE_DEPS = ["@stackage//{}".format(it) for it in PACKAGES]

EXTENSIONS = ["-XOverloadedStrings", "-XTupleSections"]

GHCOPTS = ["-threaded", "-rtsopts", "-O2"] + EXTENSIONS

def aoc(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = ["@//:aoclib"] + STACKAGE_DEPS,
        data = native.glob(["{}/*.txt".format(day)]),
        ghcopts = select({
            "//:profiling": GHCOPTS + ["-prof", "-fprof-late"],
            "//conditions:default": GHCOPTS,
        })
    )

    rust_binary(
        name = "r{}".format(day),
        srcs = ["day_{}.rs".format(day)],
        deps = ["@crate_index//:nom", "@//:aoclib-rs"],
        data = native.glob(["{}/*.txt".format(day)]),
    )
