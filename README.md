# nDim
n-Dimensional arrays in Haskell.
## Examples
With...
```haskell
import qualified Data.NDim as N
```
Generating a normally-distributed 6x6 matrix:
```haskell
import qualified Data.NDim.Random as NR
normals = fst $ NR.uniform (mkStdGen 109) 0.0 1.0 [6, 6] :: N.NDim Double
```
Adding 3.0 to every item in the matrix:
```haskell
(+ 3.0) <$> normals
```
