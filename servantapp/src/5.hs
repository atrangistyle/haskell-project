5

-- do something
fn 5 -> you didn't care whether fn is pure/impure.


m 5


--- do something  fn :: a -> _
fn (m 5)

-- questions?
fn is pure?
  fn :: a -> b -- (+1)
then
 fn <$> (m 5) -> m 6


fn is impure?
  fn :: a -> m' b -- (is this m' is same as original m)
  if and only if m == m'
    (m 5) >>= fn -- m 6 , fn =<< (m 5)
  else
     fn <$> (m 5)  -- m (m' 6)
     traverse/mapM fn (m 5) -- m' (m 6)


fn: (\x -> x + 1)
val: Maybe Int (Just 4)
-- fn <$> val
-- val (Just 5)

fn2 :: (\x -> Just (x + 1))
-- fn2 =<< val  ( Just 6)


fn3 :: (\x -> pure (x+1)) :: a -> IO b
m(m' x)

-- fn3 mapM val   pure(Just 7)


