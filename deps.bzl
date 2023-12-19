load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

PACKAGES = ["array", "base", "text", "containers", "split", "parallel", "extra",
            "attoparsec"]

def aoc(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = ["@stackage//{}".format(it) for it in PACKAGES],
        data = native.glob(["{}/*.txt".format(day)]),
        ghcopts = ["-threaded", "-rtsopts"],
    )
