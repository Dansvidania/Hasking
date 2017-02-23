import Criterion.Main

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue [] []) = Queue [] (a:[])
push a (Queue en de) = Queue (a:en) de

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue en []) = pop (Queue [] (reverse en))
pop (Queue en (d:de)) = (Just d, (Queue en de))


