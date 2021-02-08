data DataTy = DataConOne | DataConTwo | DataConRec DataTy deriving(Show)

one :: DataTy; one = DataConOne;
two :: DataTy; two = DataConTwo;
recrec :: DataTy; recrec = DataConRec recrec
recone :: DataTy; recone = DataConRec DataConOne
rectwo' :: DataTy; rectwo' = DataConRec two

main :: IO (); main = do
  print one
  print two
  print recone
  print rectwo'
  print recrec
