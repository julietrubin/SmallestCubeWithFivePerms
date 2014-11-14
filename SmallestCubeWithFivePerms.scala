import scala.collection.mutable.ArrayBuffer
    
/*
The Problem:
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.

My Solution:
Creates a hashmap where the key is the cube sorted by digits and the value is the current count of cube perms.
Key example: 0, 1, 8, 27, 46, 125, 126, 334 ...

Loops through cube roots adding to the hashmap until we find a cube with five permutations of digits that are also cubes. Since the problem asks for __exactly__ five permutations we need to check there are no more then five by looking through all the possible permutations. Returns the smallest of these cubes. 

Potential problems:
This does not count the cube 1 and 1000 as being permutations since we do not look at leading 0s. 

Optimization:
Using a trie would save memory. We are doing some repeat cube checking in getCubePerms during getSmallestCubeWithNumPerms. This could be refactored to just look at cube roots we have not seen yet. 

This is my first time writing Scala. Please excuse syntax errors. 
*/
 
object SmallestCubeWithFivePerms {
    def main(args: Array[String]) {
        val numPerms = 5 
        val smallestCube = getSmallestCubeWithNumPerms(numPerms)
        println("The smallest cube with " + numPerms + " permutations: " + smallestCube)
        testIsSmallestCubeWithNumPerms(smallestCube, numPerms)
    }
    
    def getSortedByDigits (cube:Long) : String = {
        return cube.toString.sorted
    }
    
    def getSmallestCubeWithNumPerms(numPerms:Int) : Long = {
        var cubeMap = scala.collection.mutable.HashMap.empty[String,Int]
        var cubeRoot = 0
        while (true) { 
            val cube = math.pow(cubeRoot, 3).toLong
            val cubeSortedByDigits = getSortedByDigits(cube)
            var permCount = cubeMap.getOrElse(cubeSortedByDigits, 0)
            permCount += 1
            cubeMap += (cubeSortedByDigits -> permCount)
            if (permCount == numPerms){
                // we need to check there are __exactly__ numPerms perms and no more
                // this will slow down runtime'
                val perms = getCubePerms(cubeSortedByDigits)
                if (perms.size == numPerms) {
                    return perms(0)
                }
                    
            }
            cubeRoot += 1
        }
        // this is needed or the compiler will throw an exception 
        return 0
    }
    
    def getCubePerms(cubeSortedByDigits:String): ArrayBuffer[Long] = {
        var cubes = ArrayBuffer[Long]()
        val largestPerm = cubeSortedByDigits.reverse.toDouble
        val smallestPerm = cubeSortedByDigits.toDouble
        val largestPossibleCubeRoot = math.cbrt(largestPerm).floor.toInt
        // if cubeSortedByDigits starts with 0 smallestPossibleCubeRoot will be smaller then it needs to be
        val smallestPossibleCubeRoot = math.cbrt(smallestPerm).ceil.toInt
        for (cubeRoot <- smallestPossibleCubeRoot to largestPossibleCubeRoot) {
            var cube = math.pow(cubeRoot, 3).toLong
            if (getSortedByDigits(cube) == cubeSortedByDigits) {
                cubes += cube
            }
        }
        return cubes 
    }
        
    def testIsSmallestCubeWithNumPerms(smallestCube:Long, numPerms:Int) : Boolean = {
        var cubes = getCubePerms(getSortedByDigits(smallestCube))
        if (cubes.size != numPerms) {
            println("Not exactly " + numPerms + " permutations.")
            return false
        }
        for (i <- 0 to cubes.size - 1) {
            if (math.cbrt(cubes(i)) % 1 != 0) {
                println(cubes(i) + " is not a cube.")
                return false
            }
            if (smallestCube > cubes(i)) {
                println(cubes(i) + " is not the smallest cube.")
                return false
            }
            for (j <- i + 1 to cubes.size - 1) {
                if (getSortedByDigits(cubes(i)) != getSortedByDigits(cubes(j))) {
                    println(cubes(i) + " and " + cubes(j) + " are not permutations of each other.")
                    return false
                }
                if (cubes(i) == cubes(j)) {
                    println("Repeat cubes.")
                    return false
                }
            }
        }
        println("Validation passes for cubes " + cubes.mkString(", "))
        return true
    }
}
