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

def sim (v7:*st[3]) : *st[3] =
  let v8 = gateH 1 v7
  let v9 = cntrlX 1 0 v8
  let v10 = gateZ 0 v9
  let v11 = gateZ 1 v10
  let v12 = cntrlX 1 0 v11
  let v13 = gateH 1 v12
  let v14 = swap 1 v13
  let v15 = cntrlX 1 0 v14
  in gateY 2 v15

entry test (_i:i64) =
  distmax(dist(sim (fromKet (replicate 3 0)))) |> (.0)

-- ==
-- entry: test
-- input { 0i64 }
-- output { [0i64, 0i64, 1i64] }
