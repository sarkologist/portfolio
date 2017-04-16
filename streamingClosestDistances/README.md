# compute the closest distance of trajectories to a given point
## requirements
the stack haskell build tool
`curl -sSL https://get.haskellstack.org/ | sh`
## to build
`stack build`
## to run the executable
```
cat <input-file> | stack exec shopping-exe -- -c '(x,y,z)'
```
where `<input-file>` is the csv input data, and `(x,y,z)` are the coordinates in meters of the point with respect to which we want to find the closest distances to

## to run the tests
```stack test```
