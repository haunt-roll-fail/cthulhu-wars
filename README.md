### Cthulhu Wars Solo

#### Building
In **solo** dir
```
sbt fullOptJS
```

Manually replace the content of the script tag in **online/CthulhuWarsSoloHRF_1_9.html** with the content of the file **solo/target/scala-2.13/cthulhu-wars-solo-hrf-opt.js**

In **online** dir
```
sbt "run drop-create-run cwo http://localhost:999/ 999"
```
