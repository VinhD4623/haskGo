import Lib
import Test.QuickCheck

main :: IO ()
main = do
   print "testing switchColor..."
   quickCheck prop_switchColor
   print "testing getStoneColor"
   quickCheck prop_getStoneColor
   print "testing showStone"
   quickCheck prop_showStone
   print "testing emptyBoard"
   quickCheck prop_emptyGameBoard
   quickCheck prop_getStoneFromMove

instance Arbitrary Color where
  arbitrary = elements [Black, White]

instance Arbitrary Move where
  arbitrary = do
   c <- arbitrary
   p <- elements [(x,y) | x <- [0..19], y <- [0..19]]
   return $ Place c p

-- switchColor
prop_switchColor :: Color -> Bool
prop_switchColor c = switchColor (switchColor c) == c

-- getStoneColor
prop_getStoneColor :: Stone -> Bool
prop_getStoneColor s = getStoneColor s == s

-- showStone
prop_showStone :: Stone -> Bool
prop_showStone s
  | s == Nothing  = showStone s == "-"
  | s == Just Black = showStone s == "X"
  | s == Just White = showStone s == "O"


prop_emptyGameBoard :: Int -> Bool
prop_emptyGameBoard n = emptyGameBoard n == replicate n (replicate n Nothing)

prop_getStoneFromMove :: Move -> Bool
prop_getStoneFromMove Pass = getStoneFromMove Pass == Nothing
prop_getStoneFromMove (Place color pos) = getStoneFromMove (Place color pos) == Just color

prop_getPos :: Move -> Bool
prop_getPos Pass = getPos Pass == (-1, -1)
prop_getPos (Place color pos) = getPos (Place color pos) == pos