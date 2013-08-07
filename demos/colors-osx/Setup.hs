import Distribution.Simple (simpleUserHooks, defaultMainWithHooks)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
