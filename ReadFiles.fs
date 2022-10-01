module ReadFiles

open AllocTypes
open System.IO


let input: InputData ={
    // these are for stopping the algorithm running
    BreakTime = 0
    StopTime = 100

    // set max default load
    DefaultLoad = 5

    // Penalty function a + b*N + c*S weights
    PenaltyWeights = 0.5, 2.0, 2.0
    
    // this activates the penalties to 'Maybe'
    PenaltyMaybes = false
    // when we choose to apply a cost to 'Maybe' projects
    MaybeCost = 0.0
    
    // activate maximum rank for reallocation
    MaxRankMoves = false
    // set value for maximum rank for reallocation
    RestrictAlloc = 10.5

    // true -> model1; false -> model2
    isModelOne = false
    // one run -> true; run until stationary point -> false
    oneRun = true

    // true if we want to take students in random order
    RandomOrder = true

    
    // activates the double allocation Def/Maybe system
    DoubleAllocMaybes = false
    // if we want to block the pairs allocated in first round of Def/Maybe system
    BlockSecondRound = false

    // if we want to block the pairs any time
    BlockAlloc = false

    // if we want to assign students with few preferences and supervisors
    // with a small penalty
    NonlinearPenalty = false

    // assign self-proposed projects
    SelfProposed = true

    // split projects
    SplitProjs = true
}


// just for statistics
let out: OutputData = {
    SpareDepth = 0
    MaxDepth = 0
    SumDepth = 0
    SpareCount = 0
    MaxCount = 0
    Counts = 0
    StaffLoad = [||]
}


// read from files
let sourceDirectory = __SOURCE_DIRECTORY__
let projsFile = "data\proj-details.txt"
let signoffsFile = "data\signoffs.txt"
let prefsFile = "data\prefs.txt"
let limitsFile = "data\limits.txt"
let constraintsFile = "data\constraints.txt"
let studsFile = "data\studs.txt"
let resFile = "data\projs-after.txt"
let fileRes = "data\my-allocs.txt"

let printRes = Path.Combine(sourceDirectory, fileRes)
let projsPath = Path.Combine(sourceDirectory, projsFile)
let signoffsPath = Path.Combine(sourceDirectory, signoffsFile)
let prefsPath = Path.Combine(sourceDirectory, prefsFile)
let limitsPath = Path.Combine(sourceDirectory, limitsFile)
let constraintsPath = Path.Combine(sourceDirectory, constraintsFile)
let studsPath = Path.Combine(sourceDirectory, studsFile)
let resPath = Path.Combine(sourceDirectory, resFile)


// number of projs and studs
let numProjs = 
    System.IO.File.ReadLines(projsPath) 
    |> Seq.length

let numStuds = 
    System.IO.File.ReadLines(studsPath) 
    |> Seq.length


let allowPermition = 
    System.IO.File.ReadLines(printRes)
    |> Seq.length

//---------------------------------------------------------
//--------------- BUILD DATABASE --------------------------
//---------------------------------------------------------


//---------------------------------------------------------
// num -> sid ; sid -> num
let sid2stud = Array.create numStuds 0

let stud2sid =
    System.IO.File.ReadLines(studsPath) 
    |> Seq.mapi 
        (fun i str -> 
            sid2stud.[i] <- (str |> int)
            ( (str |> int), Sid i))
    |> Map.ofSeq
    
    
//********** database.stud2Pref ************
let stud2pref: Preference list array = Array.create numStuds []

//----------------------------------------------------------------

//let mutable counts = 0
let projSupSeq =
    System.IO.File.ReadLines(projsPath) 
    |> Seq.map
        (fun str ->
            let breakline = str.Split('\t')            
            (breakline.[0] |> int, breakline.[1] |> int))

// proj -> pid
let pid2proj = Array.create numProjs 0

let numSups = 
    projSupSeq
    |> Seq.map (fun tup -> snd tup)
    |> Seq.distinct
    |> Seq.length

let stid2sup = Array.create numSups 0

let proj2pid = 
    projSupSeq
    |> Seq.mapi
        (fun i tup -> 
            //printfn "%A" (fst tup, Pid i)
            pid2proj.[i] <- fst tup
            (fst tup, Pid i))
    |> Map.ofSeq


let sup2stid = 
    projSupSeq
    |> Seq.map (fun tup -> snd tup)
    |> Seq.distinct
    |> Seq.mapi
        (fun i sup -> 
            //printfn "%A" (sup, Stid i)
            stid2sup.[i] <- sup
            (sup, Stid i))
    |> Map.ofSeq
    //|> Map.iter (fun a b -> printfn "%A, %A" a b)

    
// *********** database.Proj2Staff ************
let proj2staff = 
    projSupSeq
    |> Seq.map(fun tup ->
        sup2stid.[snd tup])
    |> Seq.toArray



// *********** database.Staff *****************
let staffArr: Supervisor array = Array.create numSups {MaxCap = input.DefaultLoad; Maybes = Map[]; NoDefs = Map[]}
 


//----------------- Build Staff No Defs -----------------------

System.IO.File.ReadLines(signoffsPath) 
|> Seq.iter (fun str ->
    let breakLine = str.Split('\t')
    if breakLine.[0] = "N" then
        let sup = breakLine.[3] |> int
        let proj = breakLine.[2] |> int
        let stud = breakLine.[1] |> int

        let stid = sup2stid.[sup]
        let pid = proj2pid.[proj]
        let sid = stud2sid.[stud]

        let oldMaybes = staffArr.[stidToInt stid].Maybes
        let oldNos = staffArr.[stidToInt stid].NoDefs


        if Map.tryFind pid staffArr.[stidToInt stid].NoDefs = None then
            let newNos = 
                oldNos
                |> Map.add pid [sid]

            staffArr.[stidToInt stid] <- {
                MaxCap = input.DefaultLoad
                Maybes = oldMaybes
                NoDefs = newNos
            }

        else
            let studLst = oldNos.[pid]
            let newStudLst = sid::studLst
            let newNos = 
                oldNos 
                |> Map.remove pid
                |> Map.add pid newStudLst

            staffArr.[stidToInt stid] <- {
                MaxCap = input.DefaultLoad
                Maybes = oldMaybes
                NoDefs = newNos

        
            }

    else if breakLine.[0] = "P" then
        let sup = breakLine.[3] |> int
        let proj = breakLine.[2] |> int
        let stud = breakLine.[1] |> int


        let stid = sup2stid.[sup]
        let pid = proj2pid.[proj]
        let sid = stud2sid.[stud]

        let oldMaybes = staffArr.[stidToInt stid].Maybes
        let oldNos = staffArr.[stidToInt stid].NoDefs
        //printfn "proj, pid, sup, stid: %A, %A, %A, %A" proj pid sup stid
        //printfn "pid: %A" pid
        //counts <- counts + 1

        if Map.tryFind pid staffArr.[stidToInt stid].Maybes = None then
            let newMaybes = 
                oldMaybes
                |> Map.add pid [sid]

            staffArr.[stidToInt stid] <- {
                MaxCap = input.DefaultLoad
                Maybes = newMaybes
                NoDefs = oldNos
            }

        else
            let studLst = oldMaybes.[pid]
            let newStudLst = sid::studLst
            let newMaybes = 
                oldNos 
                |> Map.remove pid
                |> Map.add pid newStudLst

            staffArr.[stidToInt stid] <- {
                MaxCap = input.DefaultLoad
                Maybes = newMaybes
                NoDefs = oldNos
            }
        //let isMaybe = 
        //    if Map.tryFind pid staffArr.[stidToInt stid].Maybes <> None then
        //        match staffArr.[stidToInt stid].Maybes.[pid] |> List.tryFind (fun stud -> stud = sid) with
        //        |Some x -> true
        //        |None -> false 
        //    else false

        //printfn "maybe: %A" isMaybe

        //printfn "%A, %A" stid staffArr.[stidToInt stid]
        )
//printfn "%A" counts

//----------------- Build staff max load --------------------------------
System.IO.File.ReadLines(limitsPath)
|> Seq.iteri
    (fun i str ->
        let breakLine = str.Split('\t')
        let sup = breakLine.[0] |> int
        let maxLoad = breakLine.[1] |> int
        
        let stid = sup2stid.[sup]
        let oldMaybes = staffArr.[stidToInt stid].Maybes
        let oldNos = staffArr.[stidToInt stid].NoDefs

        

        staffArr.[stidToInt stid] <- {
                    MaxCap = maxLoad
                    Maybes = oldMaybes
                    NoDefs = oldNos
                }
        )


//---------------- Build preferences lists ----------------------------

let topPrefs = Array.create numProjs false

let fstStuds = Array.create numStuds false

System.IO.File.ReadLines(prefsPath) 
|> Seq.iteri 
    (fun i str ->
        //printfn"hello"
        let breakLine = str.Split('\t')
        let stud = breakLine.[0] |> int
        let proj = breakLine.[2] |> int
        let rank = breakLine.[3] |> float

        //printfn "%A, %A, %A" stud proj rank
        
        if proj <> 0 then
            let sidInt = sidToInt stud2sid.[stud]
            let sid = stud2sid.[stud]
            let pid = proj2pid.[proj]
            let stid = proj2staff.[pidToInt pid]

            if Map.tryFind pid staffArr.[stidToInt stid].NoDefs <> None then
                let unAllowedStuds = staffArr.[stidToInt stid].NoDefs.[pid]
                let isStudInLst = 
                    unAllowedStuds
                    |> List.tryFind 
                        (fun studInLst -> sid = studInLst)

                if isStudInLst = None then 
                    stud2pref.[sidInt] <- {ProjId = pid; Rank = rank} :: stud2pref.[sidInt]
                    if rank = 1.0 (*|| rank = 2.0 || rank = 3.0 || rank = 4.0 || rank = 5.0 || rank = 6.0*) then
                        topPrefs.[pidToInt pid] <- true
                        fstStuds.[sidInt] <- true
            else  
                
                stud2pref.[sidInt] <- {ProjId = pid; Rank = rank} :: stud2pref.[sidInt]
                if rank = 1.0 (*|| rank = 2.0  || rank = 3.0 || rank = 4.0 || rank = 5.0 || rank = 6.0*) then
                        topPrefs.[pidToInt pid] <- true
                        fstStuds.[sidInt] <- true
        )


stud2pref
|> Array.iteri 
    (fun i lst -> stud2pref.[i] <- (List.rev lst))

// ---------------------- Create individual penalties -----------
let allocPens = Array.create numStuds 0.0


// here we assign penalties
stud2pref
|> Array.iteri (fun i lst -> 
    let (a, b, c) = input.PenaltyWeights
    let n = lst.Length |> float

    let s = 
        lst
        |> List.map (fun pref ->  proj2staff.[pidToInt pref.ProjId])
        |> List.distinct
        |> List.length
        |> float

    //if n < 4 then
    //    allocPens.[i] <- (0.5 + 0.5*n)

    //else 
    if input.NonlinearPenalty = true then
        if n < 4 then
            allocPens.[i] <- (0.5 + 1.0*n)
        else
            allocPens.[i] <- (a + b*n + c*s)
    else
        allocPens.[i] <- (a + b*n + c*s)

    )



// ---------------------- SPLIT PROJS ------------------------


//splitProjs
//|> Seq.iter (fun el -> printfn "el: %A" el)

// ---------------------- Build datasets -----------------------
let database: StaticData = {
    Stud2Pref = stud2pref
    Proj2Sup = proj2staff
    Staff = staffArr
    AllocPens = allocPens
    NumStudents = numStuds
    NumProjects = numProjs
    NumSups = numSups
}

let allocated0: AllocArrays = {
    Stud2Proj = Array.create database.NumStudents None 
    Proj2Stud = Array.create database.NumProjects None
}


let orderedStuds = 
    database.AllocPens
    |> Array.toList
    |> List.mapi (fun i pen -> (i, pen))
    |> List.sortByDescending (fun pair -> snd pair)
    |> List.map (fun pair -> Sid (fst pair))




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


// students which are self-proposed will be blocked
// during allocation
let unblockedStuds =
    allocated0.Stud2Proj
    |> Array.map (fun proj -> proj = None)


// array that counts how many students have that project 
let maxAllocProjs = Array.create numProjs 1

System.IO.File.ReadLines(constraintsPath) 
|> Seq.iter (fun str ->
    let breakLine = str.Split('\t')
    let proj = breakLine.[0] |> int
    let maxSplit = breakLine.[4] |> int

    if Map.tryFind proj proj2pid <> None then
        let pid = proj2pid.[proj]
        maxAllocProjs.[pidToInt pid] <- maxSplit
        )

// stores the students who got the same project
let constraintsProjs: Map<ProjectId, StudentId option array> =
    maxAllocProjs 
    |> Array.mapi (fun i max -> (i, max))
    |> Array.filter (fun pair -> snd pair <> 1)
    |> Array.map (fun pair -> Pid (fst pair))
    |> Array.map (fun pid -> pid, (Array.create maxAllocProjs.[pidToInt pid] None))
    |> Map.ofArray


// stores all the collection types above defined
let vars: Parameters = {
    UnallocStuds = unblockedStuds
    BlockMaybes = 
        if input.DoubleAllocMaybes = true then true
        else false
    MaxAllocs = maxAllocProjs
    SplitProjs = constraintsProjs
}

