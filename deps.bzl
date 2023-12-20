load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

PACKAGES = ["array", "base", "text", "containers", "split", "parallel", "extra", "directory", "unix",
            "attoparsec"]

STACKAGE_DEPS = ["@stackage//{}".format(it) for it in PACKAGES]

GHCOPTS = ["-threaded", "-rtsopts", "-O2"]

def aoc(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = ["@//:aoclib"] + STACKAGE_DEPS,
        data = native.glob(["{}/*.txt".format(day)]),
    )
