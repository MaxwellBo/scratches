-- :t sequence
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

-- Typically traversables are things that you can iterate over in some way
-- They're more specific than Functors though:
-- Examples:
-- - [] (List in Scala)
-- - Maybe (Option in Scala)
-- - Either a (Either[E, ?] in Scala)
--
-- So what does sequence do? It's kind like a, "flip inside out"

sequence [ Just 5, Just 6, Just 7 ]
-- Just [5,6,7]

-- But if one of the Maybe's in the list is a None, the whole thing resolves to a None

sequence [ Just 5, Just 6, Just 7 ]
-- None

-- So let's look at the implementation of sequence. It's pretty hairy, but I'll see if I can explain it

instance Traversable [] where
    sequence = List.foldr cons (pure [])
--                              ^ make an empty Applicative context (like Just [])
--                   ^ and progressively fold every element in
--                   ^ because our append is now in the applicative context
--                   ^ it gets the associated effects
      where cons x ys = liftA2 (:) x ys
--          ^ promote list prepending into the applicative context

-- This looks like a pretty scary definition
--
mobileClients.mobileClients.values
              .foldRight(FreeAp.lift(GetDeviceCredentialsWithoutClientApplication(userId)).map(_.map(DeviceCredential.fromAuth0)))({
                case (mobileClient, res) =>
                  (res |@| FreeAp.lift(GetMobileDeviceCredentials(userId, mobileClient.clientId)))

-- But it's very similar to what you did here
-- Let's try with a different monad
sequence [ getLine, getLine ]
-- <TYPE STUFF>
-- IO [ "hello", "world" ]
-- We succesfully transformed [IO String] into IO [String]


--So, what is traverse?

travarse f xs = fmap f xs |> sequence

-- So, a map and then immediately a sequence
-- The most common use case for this is firing off some sort of effect for every element in a list
--
traverse putStrLn [ "need to", "print this" ]

-- does exactly what you expect

-- traverse also has a flipped (takes args in the opposite order) version called forM

forM ["need to", "print this"] \i -> do
  putStrLn i


-- so we can start to make some imperative looking code. We can go one step further and use ifM

forM ["need to", "print this"] \i -> do
  ifM (i != "don't print me") do
    putStrLn i

-- to really freak people out
