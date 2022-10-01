
module Program

open System.IO
open Expecto
open AllocTypes
open Database
open ReadFiles
open Defs


// -------------------------------------------------------
// ------------------- MODE OF RUNNING -------------------
// -------------------------------------------------------

let mutable minSumRanks = 0.0
let mutable minPenalties = 0.0

let mutable newSumRanks = 0.0
let mutable newPenalties = 0.0

// this stores the objective function for every run
let costValues: (float * float) list array = Array.create 1 []


let mutable iters = 1
let staffLoad = Array.create database.NumSups 0

let finalAlloc =
    // if only one run
    if input.oneRun = true then
        
        let firstAlloc = allocStudents database allocated0 staffLoad

        // a second round of allocation if double allocation activated
        if input.DoubleAllocMaybes = true then
        
            if input.BlockSecondRound = true then
                vars.UnallocStuds
                |> Array.iteri (fun i proj -> 
                    vars.UnallocStuds.[i] <- (allocated0.Stud2Proj.[i] = None))

            vars.BlockMaybes <- false

            let newStaff = 
                database.Staff
                |> Array.map (fun sup ->
                        
                        {
                            MaxCap = sup.MaxCap + 1
                            Maybes = sup.Maybes
                            NoDefs = sup.NoDefs
                        })

            let newData = {
                database with Staff = newStaff
                }

            let newQueue =
                firstAlloc.Stud2Proj
                |> Array.mapi (fun i projOpt -> (i, projOpt))
                |> Array.filter (fun pair -> (snd pair) = None)
                |> Array.map (fun pair -> Sid (fst pair))
                |> Array.toList

            printfn "Remaining students: %A" newQueue.Length
            System.Threading.Thread.Sleep(1000)

            tryAllocStudent newData allocated0 staffLoad newQueue

        else
            firstAlloc


    else
        // done for finding the minimum point
        let rec convergeFunc (minStuds: ProjectId option list) (minProjs: StudentId option list) (load: int array)
                                (minRanks: float) (minPens: float)
                                (objFunc: float) (numStartPoints: int) =
     
             printfn "--------------------"
             printfn "countssss: %A" numStartPoints
             printfn "--------------------"

             if numStartPoints >= 2 then
                minStuds, minProjs

             else
                
                // all the following collection types are initialised
                // every time when there is a new starting point
                // ----------------------------------- STARTS HERE
                allocated0.Stud2Proj
                |> Array.iteri (fun i el -> allocated0.Stud2Proj.[i] <- None)

                allocated0.Proj2Stud
                |> Array.iteri (fun i el -> allocated0.Proj2Stud.[i] <- None)

                let newStaffLoad = Array.create database.NumSups 0

                if input.SelfProposed = true then
                    System.IO.File.ReadLines(projsPath) 
                    |> Seq.iter (fun str ->
                        let breakline = str.Split('\t') 
                        if breakline.[2] <> "" then
                            let sid = stud2sid.[(breakline.[2] |> int)]
                            let pid = proj2pid.[(breakline.[0] |> int)]

                            //printfn "%A, %A" sid pid 

                            allocated0.Stud2Proj.[sidToInt sid] <- (Some pid)
                            allocated0.Proj2Stud.[pidToInt pid] <- (Some sid)
                            )

                vars.SplitProjs
                |> Map.iter (fun proj studs ->
                    vars.SplitProjs.[proj]
                    |> Array.iteri (fun i el ->
                        vars.SplitProjs.[proj].[i] <- None
                        )
                    )

                vars.MaxAllocs
                |> Array.iteri (fun i el ->
                    vars.MaxAllocs.[i] <- 1)

                System.IO.File.ReadLines(constraintsPath) 
                |> Seq.iter (fun str ->
                    let breakLine = str.Split('\t')
                    let proj = breakLine.[0] |> int
                    let maxSplit = breakLine.[4] |> int

                    if Map.tryFind proj proj2pid <> None then
                        let pid = proj2pid.[proj]
                        vars.MaxAllocs.[pidToInt pid] <- maxSplit
                        )
                // ----------------------------------- STOPS HERE

                while iters <= 1 do
                    printfn "------"

                    newSumRanks <- 0.0
                    newPenalties <- 0.0

                    // runs the algorithm
                    allocStudents database allocated0 newStaffLoad |> ignore

                    //calculates the new sum of ranks and penalties
                    allocated0.Stud2Proj
                    |> Array.iteri 
                        (fun i proj ->

                            match proj with
                            |None -> 
                                newPenalties <- database.AllocPens.[i] + newPenalties

                            |Some x -> 
                                let rankInt = getProjRankFromPref database (Some (Sid i)) proj |> int
                                newSumRanks <- (rankInt |> float) + newSumRanks
                                )

                    printfn "WE START NEW ALLOC"
                    printfn "min ranks: %A, min penalties: %A" minSumRanks minPenalties
                    let newLst = (newSumRanks, newPenalties) :: costValues.[0]
                    //printfn "constraints in: %A" vars.SplitProjs
                    costValues.[0] <- newLst
                    //ranksArray.[i] <- newSumRanks
                    //penArray.[i] <- newPenalties

                    // if cost decreased than we continue otherwise we stop
                    if (minSumRanks + minPenalties) > (newSumRanks + newPenalties) || (minSumRanks + minPenalties) = 0 then
                        iters <- 1

                        minSumRanks <- newSumRanks
                        minPenalties <- newPenalties

                    else
                        iters <- iters + 1
    
                    printfn "count %A" iters
                    printfn "sum ranks: %A, sum penalties: %A" newSumRanks newPenalties
                    System.Threading.Thread.Sleep(500)


                // once we stop we prepare for a new starting point of the allocation
                // if the objective function for current starting point is smaller
                // than the smallest value than continue
                // otherwise count how many times in a row we do not find a smaller objective function
                if minSumRanks + minPenalties < objFunc then
                    let newObj = minSumRanks + minPenalties
                    iters <- 1
                    minSumRanks <- 0
                    minPenalties <- 0

                    let newMinStuds = allocated0.Stud2Proj |> List.ofArray
                    let newMinProjs = allocated0.Proj2Stud |> List.ofArray
                    let newStaff = newStaffLoad

                    convergeFunc newMinStuds newMinProjs newStaff minSumRanks minPenalties newObj 1   
            
                else
                    iters <- 1
                    minSumRanks <- 0
                    minPenalties <- 0
                    convergeFunc minStuds minProjs load minRanks minPens objFunc (numStartPoints + 1) 


                

        let allocRes = convergeFunc [] [] staffLoad 1000.0 1000.0 1000.0 1

        {
            Stud2Proj = (fst allocRes) |> List.toArray
            Proj2Stud = (snd allocRes) |> List.toArray
        }



// ----------------------------------------------------------------
// ------------------------ OUTPUT --------------------------------
// ----------------------------------------------------------------

// print results in output file
let lines = 
    finalAlloc.Stud2Proj 
    |> Array.mapi (fun i proj -> 
        let rawProj, rawSup = 
            if proj <> None then
                let stid = database.Proj2Sup.[pidToInt proj.Value]
                pid2proj.[pidToInt proj.Value], stid2sup.[stidToInt stid]

            else
                0, 0

        let rawStud = sid2stud.[i]
        sprintf "%A\t%A\t%A" rawStud rawProj rawSup)
File.WriteAllLines(printRes, lines)


// ----------------------------------------------------------------
// ------------- SCRIPTING FOR ANALYSING THE RESULTS --------------
// ----------------------------------------------------------------


// -------------------- TABLE OF STUDS -------------------
let freqStuds = Array.create 11 0
let mutable funcSum = 0.0
let mutable penalties = 0.0
let mutable numStuds43 = 0
let mutable numStuds54 = 0
let mutable numStuds53 = 0

let mutable metricSmall = 0
let mutable metricMed = 0
let mutable metricBig = 0

let prefsDistrib = Array.create 11 0
let supsDistrib = Array.create 11 0


finalAlloc.Stud2Proj
|> Array.iteri 
    (fun i proj ->
        //printfn "Sid %A, %A" i proj

        let numSups =
                database.Stud2Pref.[i]
                |> List.map (fun pref ->
                    let projInt = pidToInt pref.ProjId
                    database.Proj2Sup.[projInt]
                    )
                |> List.distinct
                |> List.length

        let numPrefs = database.Stud2Pref.[i].Length


        match proj with
        |None -> 
            
            let numSups =
                database.Stud2Pref.[i]
                |> List.map (fun pref ->
                    let projInt = pidToInt pref.ProjId
                    database.Proj2Sup.[projInt]
                    )
                |> List.distinct
                |> List.length

            let numPrefs = database.Stud2Pref.[i].Length

            if numPrefs >= 5 && numSups >= 3 then
                numStuds53 <- numStuds53 + 1
                
            if numPrefs >= 5 && numSups >= 4 then
                numStuds54 <- numStuds54 + 1

            if numPrefs >= 4 && numSups >= 3 then
                numStuds43 <- numStuds43 + 1

            //printfn "stud unalloc: %A. num prefs %A" i database.Stud2Pref.[i].Length
            freqStuds.[10] <- 1 + freqStuds.[10]
            penalties <- database.AllocPens.[i] + penalties

        |Some x -> 

            //printfn "num Prefs: %A, num Sups: %A" database.Stud2Pref.[i].Length numSups
            
            let rankInt = getProjRankFromPref database (Some (Sid i)) proj |> int
            funcSum <- (rankInt |> float) + funcSum
            if rankInt > 4 then
                printfn "stud bad rank: %A, rank %A" i rankInt
            if rankInt = 0 then
                printfn "strange.. should not happen"
                freqStuds.[0] <- 1 + freqStuds.[0]
            else
                freqStuds.[rankInt - 1] <- 1 + freqStuds.[rankInt - 1])

printfn "-------"

// --------------------- TABLE OF STAFF --------------------
let freqSups = Array.create 11 0




// --------------------- UNFAIRNESS PER STUD ---------------
let mutable unfairAllocsFst = 0
let mutable unfairAllocsTotal = 0

//allocated0.Stud2Proj
finalAlloc.Stud2Proj
|> Array.iteri (fun i pid ->
    if pid <> None then
        let allocRank = getProjRankFromPref database (Some (Sid i)) pid
        
        //let isAnyfstRank = 
        if allocRank <> 1.0 then
            //printfn "Sid %A, Rank %A\n" i allocRank
            let fstPrefs =
                database.Stud2Pref.[i]
                |> List.filter (fun pref -> pref.Rank = 1.0)
                |> List.filter (fun pref ->
                    let isPrefAllocated = finalAlloc.Proj2Stud.[pidToInt pref.ProjId]
                    let supInt = stidToInt database.Proj2Sup.[pidToInt pref.ProjId]
                    let sup = database.Staff.[supInt]
                    isPrefAllocated <> None || staffLoad.[supInt] < sup.MaxCap)

            if fstPrefs <> List.empty then

                fstPrefs
                |> List.iter (fun pref ->
                    let allocatedSidToPref = finalAlloc.Proj2Stud.[pidToInt pref.ProjId]
                    let prefAllocRank = getProjRankFromPref database allocatedSidToPref (Some pref.ProjId)
                    //printfn "pref: %A\n" prefAllocRank
                    if prefAllocRank <> 1.0 then
                        unfairAllocsFst <- unfairAllocsFst + 1
                    )

    )

//allocated0.Stud2Proj
finalAlloc.Stud2Proj
|> Array.iteri (fun i pid ->
    if pid <> None then
        let allocRank = getProjRankFromPref database (Some (Sid i)) pid
        
        //let isAnyfstRank = 
        let fstPrefs =
            database.Stud2Pref.[i]
            |> List.filter (fun pref -> pref.Rank < allocRank)
            |> List.filter (fun pref ->
                let isPrefAllocated = finalAlloc.Proj2Stud.[pidToInt pref.ProjId]
                let supInt = stidToInt database.Proj2Sup.[pidToInt pref.ProjId]
                let sup = database.Staff.[supInt]
                isPrefAllocated <> None || staffLoad.[supInt] < sup.MaxCap)

        if fstPrefs <> List.empty then
            //printfn "Sid %A, Rank %A" i allocRank
            //printfn "prefs: %A" fstPrefs
            //let findFair =
            fstPrefs
            |> List.iter (fun pref ->
                let allocatedSidToPref = finalAlloc.Proj2Stud.[pidToInt pref.ProjId]
                let prefAllocRank = getProjRankFromPref database allocatedSidToPref (Some pref.ProjId)
                //printfn" %A, %A " prefAllocRank pref.Rank
                if prefAllocRank > pref.Rank || prefAllocRank = 0.0 then 
                    
                    unfairAllocsTotal <- unfairAllocsTotal + 1
                )
            //if findFair = None then
            //    unfairAllocsTotal <- unfairAllocsTotal + 1
    )


// ----------------------------------------------------------------
// ------------------------ DEBUGING ------------------------------
// ----------------------------------------------------------------


//allocated0.Stud2Proj
//|> Array.mapi (fun i proj -> (i, proj))
//|> Array.filter (fun pair -> snd pair = None)
//|> Array.iteri (fun i pair ->
//    let proj = snd pair
//    //printfn"------"
//    let currStud = Sid (fst pair)
//    //let currProj = allocated
//    //printfn "currStud: %A" currStud
//    database.Stud2Pref.[sidToInt currStud]
//    |> List.iter (fun pref ->
//        let projInt = pidToInt pref.ProjId
//        let supInt = stidToInt database.Proj2Sup.[projInt]
//        let sup = database.Staff.[supInt]
//        let stud = allocated0.Proj2Stud[projInt]

//        let newStudRank = getProjRankFromPref database (Some currStud) (Some pref.ProjId)
//        //let isMoveAllowed = 
//        //    if vertex.Alloc.Depth <> 0 then
//        //        newStudRank < input.RestrictAlloc
//        //    else 
//        //        if mode1 = 1 then
//        //            newStudRank < input.RestrictAlloc
//        //        else if mode1 = 2 then
//        //            newStudRank > input.RestrictAlloc
//        //        else
//        //            failwithf "allocation mode does not exist (simpleBFS)"


//        // we eliminate current node
//        //let isNotCurrProj = Some (pref.ProjId) <> vertex.Alloc.Proj

//        // check if staff is loaded
//        let isNotLoaded = 
//            if stud = None then
//                (staffLoad.[supInt] < sup.MaxCap)
//            else
//                true

//        // check if student is approved for this project
//        let isApproved = 
//            if Map.tryFind (Pid projInt) sup.NoDefs <> None then
//                match sup.NoDefs.[Pid projInt] |> List.tryFind (fun stud -> stud = currStud) with
//                |Some x -> false
//                |None -> true 
//            else true
//        printf"."

//        //printfn "rank: %A, %A" pref.Rank pref.ProjId
//        //printfn " notLoad, approved: %A, %A\n"  isNotLoaded  isApproved  (*isMoveAllowed*)
//        ))

// count maybes
let mutable maybes = 0
let mutable topMaybes = 0

//allocated0.Stud2Proj
finalAlloc.Stud2Proj
|> Array.mapi (fun i proj -> (i, proj))
|> Array.filter (fun pair -> snd pair <> None)
|> Array.iter (fun pair ->
    let currStud = Sid (fst pair)
    let proj = snd pair
    let projInt = pidToInt proj.Value
    let supInt = stidToInt database.Proj2Sup.[projInt]
    let sup = database.Staff.[supInt]


    let isNotMaybe = 
        if Map.tryFind proj.Value sup.Maybes <> None then
            match sup.Maybes.[Pid projInt] |> List.tryFind (fun stud -> stud = currStud) with
            |Some x -> false
            |None -> true 
        else true

    if isNotMaybe = false then
        printfn "maybe num prefs: %A" database.Stud2Pref.[fst pair].Length 
                    
        if (getProjRankFromPref database (Some currStud) proj) < 3.5 then
            topMaybes <- topMaybes + 1
        //printfn "allocated: %A" allocated0.Stud2Proj.[i]
        maybes <- maybes + 1
    )

printfn" \ntesting"
//allocated0.Stud2Proj
//finalAlloc.Stud2Proj
//|> Array.iteri (fun i proj ->
//    //printfn "stud %A, %A" i proj
//    //printfn "rank %A" (getProjRankFromPref database (Some (Sid i)) proj)
//    if proj <> None then
//        if proj.Value <> Pid 35 && 
//            proj.Value <> Pid 40 && 
//            proj.Value <> Pid 47 && 
//            proj.Value <> Pid 48 && 
//            proj.Value <> Pid 58 && 
//            proj.Value <> Pid 62 && 
//            proj.Value <> Pid 63 && 
//            proj.Value <> Pid 67 && 
//            proj.Value <> Pid 107 && 
//            proj.Value <> Pid 109 && 
//            proj.Value <> Pid 151 then

//            let result =
//                finalAlloc.Stud2Proj
//                |> Array.removeAt i
//                |> Array.tryFind (fun pid -> 
//                    //printfn "stud %A, %A" i proj
//                    pid = proj)
//            if result <> None then
//                failwithf "allocated project twice\n")


finalAlloc.Proj2Stud
|> Array.iteri (fun i stud ->
    //printfn "proj %A, %A" i stud
    if stud <> None then
        let result =
            finalAlloc.Proj2Stud
            |> Array.removeAt i
            |> Array.tryFind (fun sid -> 
                sid = stud)
        if result <> None then
            failwithf "allocated student twice\n")


staffLoad
|> Array.iter (fun load ->
    if load < 11 then
        freqSups.[load] <- 1 + freqSups.[load]
    else if load < 0 then
        failwithf "error with staff load")


//printfn "hei : %A" ((freqStuds.[0] + freqStuds.[1] + freqStuds.[2])/ ((Array.sum freqStuds) - 3))

let top3Studs = ((freqStuds.[0] + freqStuds.[1] + freqStuds.[2] ) |> float) / ((numStuds - 3) |> float) * 100.0
let top4Studs = ((freqStuds.[0] + freqStuds.[1] + freqStuds.[2] + freqStuds.[3]) |> float) / ((numStuds - 3) |> float) * 100.0
let top3Sups = ((freqSups.[0] + freqSups.[1] + freqSups.[2]) |> float) / ((Array.sum freqSups)|> float) * 100.0
    

// ----------------------------------------------------------------
// ------------------------ PRINTING ------------------------------
// ----------------------------------------------------------------


printfn "\nStudents Table       Staff table"
for i in 0..10 do
    if i <> 10 then
        printfn "rank: %A, -> %A\t; load: %A, -> %A" (i+1) freqStuds.[i] (i) freqSups.[i]
    else 
        printfn "Unalloc: -> %A\t; load: %A, -> %A" freqStuds.[i] (i) freqSups.[i]

printfn "penalty: %A" input.PenaltyWeights

printfn "----"

costValues.[0]
|> List.rev
|> List.iteri (fun i cost ->
    printfn "allocation %A, objFunc: %A pens: %A" i (fst cost) (snd cost))

printfn "----"

printfn "top 3 stud: %A" top3Studs

//printfn "top 4 stud: %A" top4Studs

printfn "----"

printfn "unfair 1st allocations: %A,\n unfair total allocations: %A" unfairAllocsFst unfairAllocsTotal

printfn "----"

printfn " unalloc >= 5 and 3: %A"  numStuds53  

printfn "----"

printfn "obj func: %A, Penalties: %A\n" funcSum penalties

printfn "----"

printfn "maybes %A" maybes

printfn "top maybes %A" topMaybes

printfn "----"

//printfn "top 3 sup: %A" top3Sups

//printfn "----"

//let avgCost = (((database.AllocPens |> Array.sum) - 13.5) / 146.0)

//printfn "avg pen cost %A" avgCost

//printfn "norm cost %A" (penalties/avgCost)

//printfn "avg count, max count: %A, %A" ((out.Counts|> float)/((database.NumStudents - freqStuds.[10]) |> float)) out.MaxCount

//printfn "avg depth, max depth: %A, %A" ((out.SumDepth |> float) /((database.NumStudents - freqStuds.[10]) |> float)) out.MaxDepth



//------------ OLD RESULTS --------------------
//let oldresults = []

//let oldStudsFreq = Array.create 11 0

//let oldLoad = Array.create database.NumSups 0

//System.IO.File.ReadLines(resPath)
//|> Seq.iteri
//    (fun i str ->
//        let breakline = str.Split('\t')
//        let proj = breakline.[0] |> int
//        let sup = breakline.[1] |> int
//        let stud = breakline.[2] |> int

//        //printfn "%A, %A, %A" proj sup stud
        
//        let pid = Map.tryFind proj proj2pid
//        let stid = Map.tryFind sup sup2stid
//        let sid = Map.tryFind stud stud2sid

//        match sid, stid, pid with
//        |Some studID, _, Some projID ->

//            let rank = getProjRankFromPref database (Some studID) (Some projID) |> int

//            if rank = 0 then
//                //printfn "strange"
//                oldStudsFreq.[10] <- 1 + oldStudsFreq.[10]
                
//            else 
//                oldStudsFreq.[rank-1] <- 1 + oldStudsFreq.[rank-1]
//        | _, _, _ -> printfn "skip"
//        )


//printfn "Staff"
//for i in 0..10 do
//    printfn "%A, %A" (i) oldStudsFreq.[i]










//----------------------------------------------------------------------------------
    
//let revOfRevIsOrig (x: int list) = List.rev (List.rev x) = x
    
//let listsAreSmall (x:int list) = List.length x < 10 // this test should fail
    
//// Tests are written as individual Expecto Test values, each tagged with [<Tests>]
//// The code demonstrates different ways to run Expecto and FsCheck tests
 


//printfn "%A" (possibleAllocs database1 allocated1 queue1 visitedProjs1 allPaths1 solutions1)
//printfn "-------------------"
//printfn "%A" resSolutions1
 
//[<Tests>]
//let test1 =
//    testCase "test1" <| fun () ->
//    let actual1 = resSolutions1
//    Expect.equal actual1 resSolutions1 "tst1"

//[<Tests>]
//let test2 =
//    testCase "test2" <| fun () ->
//    let actual2 = possibleAllocs database1 allocated1 queue1 visitedProjs1 allPaths1 solutions1
//    Expect.equal actual2 resSolutions1 "tst2"
    

    
//let testList =
//    testList "A test group" [
//        test1
//        test2
//        ]
    
//let testsWithExpecto() =
//    runTests defaultConfig testList |> ignore
//    //runTestsWithCLIArgs [] [||] testList
    
////let allTestsWithExpecto() =
////    runTestsInAssembly defaultConfig [||]
    
//[<EntryPoint>]
//let main argv =
//    printfn "Testing with FSCheck and Expecto!"
//    //testsWithoutExpecto() |> ignore
//    testsWithExpecto() |> ignore
//    //allTestsWithExpecto() |> ignore
//    Console.ReadKey() |> ignore
//    0 // return an integer exit code



// ---------------------- TRASH -------------------------------

//printfn "proj int: %A" projInt
                        //printfn "is allowed: %A" isAllowed

                        //if (now() - initialTime) > 100000 && (now() - initialTime) < 150000 then
                        //    printfn "loop1"

                        //if checks = true then
                        //    printf " %A, " projInt

                        //    printfn ""
                        //    printfn "isNotCurrProj, isNotLoaded, isAllowed %A, %A, %A" isNotCurrProj isNotLoaded isAllowed
