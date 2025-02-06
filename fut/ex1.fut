-- Circuit for c = (I ** H ++ CX ++ Z ** Z ++ CX ++ I ** H) ** I ++ I ** SW ++ CX ** Y:
--               .---.
-- ----------*---| Z |---*-----------------*----
--           |   '---'   |                 |
--           |           |                 |
--   .---. .-+-. .---. .-+-. .---.       .-+-.
-- --| H |-| X |-| Z |-| X |-| H |-.   .-| X |--
--   '---' '---' '---' '---' '---'  \ /  '---'
--                                   /
--                                  / \  .---.
-- --------------------------------'   '-| Y |--
--                                       '---'
-- Futhark simulation function for c:
--
import "qsim"
open gates

def sim (v:*st[3]) : *st[3] =
  let v = gateH 1 v
  let v = cntrlX 1 0 v
  let v = gateZ 0 v
  let v = gateZ 1 v
  let v = cntrlX 1 0 v
  let v = gateH 1 v
  let v = swap 1 v
  let v = cntrlX 1 0 v
  in gateY 2 v

entry test (_i:i64) =
  distmax(dist(sim (fromKet [1,0,1]))) |> (.0)

-- ==
-- entry: test
-- input { 0i64 }
-- output { [1i64, 0i64, 0i64] }
