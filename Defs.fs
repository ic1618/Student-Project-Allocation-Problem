module Defs

open AllocTypes
open Database
open ReadFiles



//----------------------------------------------------------------
//----------------------DATA VERIFICATION-------------------------
//----------------------------------------------------------------

/// checks if the database is synchronised in terms of number of students, projects, sups
let checkSyncData (data:StaticData) (allocated:AllocArrays) (inp: InputData) = 
    let checkData = 
        (data.Stud2Pref.Length = data.NumStudents) &&
        (data.Proj2Sup.Length = data.NumProjects) &&
        (data.Staff.Length = data.NumSups)
    
    let checkAlloc =
        (allocated.Stud2Proj.Length = data.NumStudents) &&
        (allocated.Proj2Stud.Length = data.NumProjects)

    if inp.PenaltyMaybes && inp.DoubleAllocMaybes = true then
        failwithf "cannot have two types of defs/maybes systems"

    match checkData, checkAlloc with
    |true, true -> printfn "data synchronised\n\n"
    |true, false -> failwithf "error with database\n"
    |false, true -> failwithf "error with allocated\n"
    |false, false -> failwithf "error with both allocated and database\n"



//----------------------------------------------------------------
//----------------------IMPLEMENTATION----------------------------
//----------------------------------------------------------------

/// for measuring elapsed time
let now : unit -> int64 =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start ()
    fun () -> sw.ElapsedMilliseconds


/// generate a list with distinct random numbers within a range
/// select count smaller or equal than NumStudents
let genRandomNumbers count numStuds =
    let rnd = System.Random()
    let initial = Seq.initInfinite (fun _ -> rnd.Next (0, numStuds)) 
    initial
    |> Seq.distinct
    |> Seq.take(count)
    |> Seq.toList

/// get a random number within a range
let getRandomNum range = 
    let rnd = System.Random()
    let randNum = rnd.Next (range)
    randNum 
    
/// just testing purposes, generates a list of preferences for students 
let rec genStudsPrefs (nrStuds: int) (nrProjs: int) (min: int) (max: int) =
    let studsPrefs: int list array = Array.create nrStuds [] 
    studsPrefs 
    |> Array.iteri (fun i el ->
        let mutable sizeLst = getRandomNum (max + 1)
        while sizeLst < min do
            sizeLst <- getRandomNum (max + 1)
        studsPrefs.[i] <- (genRandomNumbers sizeLst nrProjs))
    studsPrefs


/// get rank of project from a student list of preferences by knowing project ID
let getProjRankFromPref (data: StaticData) (stud: StudentId option) (proj: ProjectId option): float =
    if stud = None then
        0.0
    else if proj = None then
        0.0
    else
        let pref = data.Stud2Pref.[sidToInt stud.Value] |> List.tryFind (fun pref -> pref.ProjId = proj.Value)
        if pref <> None then pref.Value.Rank 
        else if input.SelfProposed = true then
            if vars.UnallocStuds.[sidToInt stud.Value] = false then
                1.0
            else 0.0
        else
            0.0

//let newRanks (data: StaticData) (stud: StudentId option) (proj: ProjectId option): float =
//    if stud = None then
//        0.0
//    else if proj = None then
//        0.0
//    else
//        let pref = data.Stud2Pref.[sidToInt stud.Value] |> List.tryFind (fun pref -> pref.ProjId = proj.Value)
//        if pref <> None then 
//            pref.Value.Rank * pref.Value.Rank/2.0
//            //if pref.Value.Rank < 4 then
//            //    pref.Value.Rank 
//            //elif pref.Value.Rank >= 4 && pref.Value.Rank <= 6 then
//            //    pref.Value.Rank + 2.0
//            //else
//            //    pref.Value.Rank + 4.0
//        else if inp0.SelfProposed = true then
//            if inp.Parameters.UnallocStuds.[sidToInt stud.Value] = false then
//                1.0
//            else 0.0
//        else
//            0.0

/// get rank of project when we know ONLY the project ID
/// A FUNCTION NOT USED ANYMORE
let getAllocProjRank (data: StaticData) (allocated: AllocArrays) (proj: ProjectId option): float =
    getProjRankFromPref data allocated.Proj2Stud.[pidToInt proj.Value] proj


/// all possible nodes
let nodesConfig (node: Alloc): int =
    match node.Stud, node.Proj with
        |Some x, Some y -> 0 // (s,p)
        |Some x, None   -> 1 // (s, )
        |None, Some y   -> 2 // ( ,p)
        |None, None     -> 3 // ( , )


/// all possible combinations of two student-project pairs
/// this function is important because we need to know previous node
/// in order to calculate the cost of a permutation
let possibPairsNodes (parentNode: Alloc) (childNode: Alloc): int =
    match (parentNode.Stud, parentNode.Proj), (childNode.Stud, childNode.Proj) with
            |(Some x, Some y), (Some z, Some w) -> 0 // (s/p) -> (s/p)
            |(Some x, Some y), (None, Some w)   -> 1 // (s/p) -> ( /p)
            |(Some x, None), (Some z, Some w)   -> 2 // (s/ ) -> (s/p)
            |(Some x, None), (None, Some w)     -> 3 // (s/ ) -> ( /p)
            |(_, _), (_, _)                     -> 4  // should not happen


/// finds if the project is a maybe for a student
/// if penaltyMaybes activated
let findMaybe (data: StaticData) (stud: StudentId option) (proj: ProjectId option): bool = 
    
    if proj <> None && stud <> None && input.PenaltyMaybes = true then
        let projInt = pidToInt proj.Value
        let supInt = stidToInt data.Proj2Sup.[projInt]
        let sup = data.Staff.[supInt]

        if Map.tryFind (Pid projInt) sup.Maybes <> None then
            match sup.Maybes.[Pid projInt] |> List.tryFind (fun student -> student = stud.Value) with
            |Some x -> true
            |None -> false 
        else false
        
    else
        false


/// update cost of path if new node is added to a path
/// we suppose that the last node in the path is the new node
/// works only for reversed node lists
let movesPathCost (data: StaticData) (newPair: Alloc) (oldPath: Node list) : Cost =

    let isStudPref = true 

    match oldPath with
    |fst::snd::lst  -> // if a node is added to an already built list
        let childNode, parentNode =
            match isStudPref with
            |true    -> newPair, oldPath.[0].Alloc
            |false   -> (List.last oldPath).Alloc, newPair

        let newMaybeCost = 
            if findMaybe data parentNode.Stud childNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        let prevMaybeCostParent =
            if findMaybe data parentNode.Stud parentNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        let prevMaybeCostChild =
            if findMaybe data childNode.Stud childNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        // calculates the cost for every possible combination of nodes (> 2 nodes)
        let newCost, currCost, prevCost =
            match isStudPref, (possibPairsNodes parentNode childNode) with
            |true, 0 -> // (s/p) -> (s/p)
                (getProjRankFromPref data parentNode.Stud childNode.Proj) +
                newMaybeCost,
                (getProjRankFromPref data childNode.Stud childNode.Proj) +
                prevMaybeCostChild,
                oldPath.[0].Cost.Seq

            |true, 1 -> // (s/p) -> ( /p) 
                (getProjRankFromPref data parentNode.Stud childNode.Proj)
                + newMaybeCost,
                0.0,
                oldPath.[0].Cost.Seq

            |false, 0 -> // (s/p) -> (s/p) // not functional yet
                failwithf "updatePathCost case not implemented yet"
                (getProjRankFromPref data parentNode.Stud childNode.Proj),
                (getProjRankFromPref data parentNode.Stud parentNode.Proj),
                (List.last oldPath).Cost.Seq

            |false, 2 -> // (s/ ) -> (s/p) // not functional yet
                failwithf"updatePathCost case not implemented yet"
                (getProjRankFromPref data parentNode.Stud childNode.Proj),
                0.0,
                (List.last oldPath).Cost.Seq     

            |_, _            -> // N/A case
                failwithf "updatePathCost more than 2 nodes error"


        {Shift = newCost - currCost; Seq = newCost - currCost + prevCost}


    // calculates the when a new path is created (2 nodes)
    |fst::lst       -> // if there is a just created list made of two nodes

        let childNode, parentNode = 
            match isStudPref with
            |true   -> newPair, oldPath.[0].Alloc
            |false  -> oldPath.[0].Alloc, newPair

        let newMaybeCost = 
            if findMaybe data parentNode.Stud childNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        let prevMaybeCostParent =
            if findMaybe data parentNode.Stud parentNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        let prevMaybeCostChild =
            if findMaybe data childNode.Stud childNode.Proj = true then
                input.MaybeCost
            else 
                0.0

        let newCost, currCost = 
            match (possibPairsNodes parentNode childNode) with
            |0   ->  // (s/p) -> (s/p)
                (getProjRankFromPref data parentNode.Stud childNode.Proj) + 
                newMaybeCost,

                (getProjRankFromPref data parentNode.Stud parentNode.Proj) +
                (getProjRankFromPref data childNode.Stud childNode.Proj) + 
                prevMaybeCostParent + prevMaybeCostChild
                
            |1   ->  // (s/p) -> ( /p)
                (getProjRankFromPref data parentNode.Stud childNode.Proj) +
                newMaybeCost,
                (getProjRankFromPref data parentNode.Stud parentNode.Proj) +
                prevMaybeCostParent

            |2   ->  // (s/ ) -> (s/p)
                (getProjRankFromPref data parentNode.Stud childNode.Proj) +
                newMaybeCost,
                (getProjRankFromPref data childNode.Stud childNode.Proj) + 
                prevMaybeCostChild
                
            |3   ->  // (s/ ) -> ( /p)
                (getProjRankFromPref data parentNode.Stud childNode.Proj) +
                newMaybeCost,
                0.0
            |_          ->    // cases that should not happen
                failwithf "updatePathCost no of nodes = 2 error"

        let moveCost = newCost - currCost (*+ caseCost*)
        {Shift = moveCost; Seq = moveCost}


    |_              -> // empty list - this case should not happen though
        {Shift = 0.0; Seq = 0.0}    


/// include penalties for unallocated students (in addition to movesPathCost)
let totalPathCost (data: StaticData) (vertex: Node) (newNode: Node) =
    
    let prevMaybeCostParent =
            if findMaybe data vertex.Alloc.Stud vertex.Alloc.Proj = true then
                input.MaybeCost
            else 
                0.0

    let penalty = 
        match possibPairsNodes vertex.Alloc newNode.Alloc with
        |0 -> //(s/p) -> (s/p)
            let newStud = sidToInt newNode.Alloc.Stud.Value
            //let vertexStud = sidToInt vertex.Alloc.Stud.Value

            if vertex.Alloc.Stud <> newNode.Alloc.Stud then
                data.AllocPens.[newStud] (*- data.AllocPens.[vertexStud]*)
            else 
                getProjRankFromPref data vertex.Alloc.Stud vertex.Alloc.Proj +
                prevMaybeCostParent

        |1 -> //(s,p) -> ( ,p)
            0.0

        |2 -> //(s, ) -> (s,p)
            let newStud = sidToInt newNode.Alloc.Stud.Value
            let vertexStud = sidToInt vertex.Alloc.Stud.Value

            data.AllocPens.[newStud] - data.AllocPens.[vertexStud]

        |3 -> //(s, ) -> ( ,p)
            let vertexStud = sidToInt vertex.Alloc.Stud.Value

            0.0 - data.AllocPens.[vertexStud]

        |_ -> failwithf "totalPathCost error"

    newNode.Cost.Seq + penalty


/// backtracking method to find smallest cost path from leaf node to vertex node
let createOptimalPaths (data: StaticData) (vertex: Node) (allPaths: Node list array) 
                        (filteredPaths: Node list list) =

    //let initTime = now()
    let vertexStud = vertex.Alloc.Stud

    let rec optimPath (currPath: Node list) (rebuildPath: Node list) 
                    (startStud: StudentId option): Node list =

        if currPath <> List.empty then
            let currNode = currPath.[0]
            //let tim = now() - initTime
            //if tim > 100 then
            //    printfn "currPath: %A" filteredPaths.Length

            match nodesConfig currNode.Alloc with
            |0 ->
                let sid = currNode.Alloc.Stud
                let stud = sidToInt sid.Value

                let pathToUse = 
                    if sid <> startStud && sid <> vertexStud then
                        //if currNode.Alloc.Proj = Some(Pid 4) then
                        if allPaths.[stud].Head.Cost.Seq < currNode.Cost.Seq then

                            let findRepNode =
                                currPath 
                                |> List.filter (fun node ->
                                    //printfn "each node: %A" nodeStud
                                    node <> currNode && 
                                    node.Alloc.Stud <> vertexStud)
                                |> List.tryFind (fun node ->
                                    node.Alloc.Stud = sid)
                            //printfn "findRep %A" findRepNode
                            //printfn "miau"

                            if (findRepNode = None) then
                                allPaths.[stud]

                            else currPath
                        else currPath
                    else currPath

                //printfn "pathToUse: %A" pathToUse

                let addPath = pathToUse.[0] :: rebuildPath


                match pathToUse with
                |fst :: lst -> optimPath lst addPath startStud
                |[] -> addPath
                    
            |2 -> 
                //printfn "( ,p)"
                let addPath = currNode :: rebuildPath
                let travPath = 
                    match currPath with
                    |fst::lst -> lst
                    |_ -> failwithf "error optimPath 5"

                //printfn "addPath: %A" addPath
                //printfn "travPath: %A\n" travPath

                optimPath travPath addPath startStud

            |1 -> 
                (currNode :: rebuildPath)

            |_ -> failwithf "error"
        else
            rebuildPath


    


    let costAndPath =
        filteredPaths.[0..100]
        |> List.map (fun path ->
            //printfn "-------------------" 
            let startStud = path.[0].Alloc.Stud

            (optimPath path [] startStud)(* |> List.rev *)           
            )
        |> List.map (fun path ->
            //printfn "path:: %A\n" path
            let costPath = 
                path
                //|> List.rev
                |> List.map (fun node -> node.Cost.Shift)
                |> List.mapFold (fun state elem -> let nxt = state + elem in (nxt,nxt)) 0
                |> fst
            //printfn"-----------"
            //printfn "%A\n" costPath
            
            let totalCosts =
                (costPath, path)
                ||> List.mapi2 (fun i cost node ->
                    if i > 0 then
                        let prevTotalCost = totalPathCost data vertex node
                        let newTotalCost = prevTotalCost + cost - node.Cost.Seq

                        newTotalCost
                    else 
                        0.0
                    )
            //let tim = now() - initTime
            //if tim > 100 then
            //    printfn "%A" tim
            //printfn "%A\n" totalCosts

            // some optimization can be done here taking all possible paths

            let minCost = 
                totalCosts
                |> List.removeAt 0
                |> List.min 

            //printfn "%A" minCost

            let pathLen = 
                totalCosts 
                |> List.removeAt 0
                |> List.findIndex (fun cost -> minCost = cost) 
                |> (+) 2

            //printfn "%A" pathLen
                
            let minCostPath = 
                path 
                |> List.take pathLen
                |> List.rev

            //printfn "%A" minCostPath

            minCostPath, minCost
            )

    let minCost = 
        costAndPath
        |> List.map (fun el -> snd el)
        |> List.min

    let solIndex = 
        costAndPath
        |> List.findIndex (fun el -> snd el = minCost)  

    //if minCost < 0.0 then
    //    Some (fst costAndPath.[solIndex])
    //else 
    //    None
    costAndPath.[solIndex]


/// find all the possible possible allocations for a student by doing succesive shift moves
/// similar to a breadth first search algorithm for graphs (we consider data structure 
/// being similar to a graph)

/// breath first search without repeating the nodes
let rec bfsModel2 (data: StaticData) (allocated: AllocArrays)
                  (mode: int) (depth: int) (vertex: Node) (visitedStuds: bool array)
                  (queue: Alloc list) (solution: Node list option) (staffLoad: int array)
                  (endedPaths: Node list list) (allPaths: Node list array) =

    //printfn "------ ooooo -------- "
    if queue = List.empty then
        out.SpareDepth <- depth

        solution
        //let filteredPaths = 
        //    allPaths
        //    |> Array.filter (fun path -> path.Length > depth)
        //    |> Array.toList

        //let pathsToStudy = 
        //    (filteredPaths @ endedPaths)
        //    |> List.filter (fun path -> path.Length > 1)


        ////printfn "pathsToStudy %A" pathsToStudy.Length

        //if pathsToStudy <> List.empty then

        //    let allEndProjs: Node list array = Array.create data.NumProjects []

        //    pathsToStudy
        //    |> List.iteri (fun i path ->
        //        let proj = pidToInt path.[0].Alloc.Proj.Value
        //        if allEndProjs.[proj] = List.empty then
        //            allEndProjs.[proj] <- path
        //        else
        //            if path.[0].Cost.Seq < allEndProjs.[proj].[0].Cost.Seq then
        //                allEndProjs.[proj] <- path
        //            )
        
        //    let potentialSols =
        //        allEndProjs
        //        |> Array.filter (fun path -> path <> List.empty)
        //        |> Array.toList

        //    //printfn "pathsToStudy %A" potentialSols.Length
        //    //potentialSols
        //    //|> List.iteri (fun i path -> printfn "index: %A\n, %A" i path)

        //    let optimalPath, optimalPathCost = createOptimalPaths data vertex allPaths potentialSols

        //    //printfn"optimal cost: %A" optimalPathCost
            
        //    if solution <> None then
        //        let solCost = totalPathCost data vertex solution.Value.[0]
        //        //printfn "solCost: %A" solCost
        //        //printfn "solCost: %A" solCost
        //        if optimalPathCost < solCost then
        //            //printfn "cost3: %A" optimalPathCost
        //            Some optimalPath
        //        else
        //            //printfn "efficient"
        //            solution
        //    else
        //        //printfn "cost3: %A" optimalPathCost
        //        Some optimalPath
        //else 
        //    if solution <> None then
        //        //printfn "efficient"
        //        solution
        //    else
        //        None
        
        
        

    else
        // ---------- variables for categorising the bfs cases ---------
        //--------------------------------------------------------------
        let currAlloc = queue.[0]
        let mutable sol = solution

        let mutable checks = false

        if currAlloc.Depth = 0 then
            out.SpareCount <- 0
            out.SpareDepth <- 0

        let mutable i = 0

        let isVisited = 
            if nodesConfig currAlloc = 2 then false
            else
                visitedStuds.[sidToInt currAlloc.Stud.Value]
       
        // see if this node is allowed to produce child nodes
        match nodesConfig currAlloc, isVisited with
        |0, false |1, false -> // (s,p) (s, )
            let currPath = allPaths.[sidToInt currAlloc.Stud.Value]
            let currStud = currAlloc.Stud.Value

            visitedStuds.[sidToInt currStud] <- true

            let initialTime = now()

            let newAllocsOpt = 
                data.Stud2Pref.[sidToInt currStud] // preferences of the current student
                |> List.map (fun pref ->
                    // a preference can be considered multiple times if the project is split
                    if Map.tryFind pref.ProjId vars.SplitProjs = None then
                        [pref]
                    else
                        List.replicate vars.SplitProjs.[pref.ProjId].Length pref)
                |> List.concat
                |> List.map (fun pref -> 
                    
                    // -------------- FILTER CONDITIONS ---------------------
                    // we constrain moves and keep only good child nodes

                    // parameters we need
                    let projInt = pidToInt pref.ProjId
                    let supInt = stidToInt data.Proj2Sup.[projInt]
                    let sup = data.Staff.[supInt]

                    // find student of child node
                    let stud = 
                        if input.SplitProjs = true then
                            if Map.tryFind pref.ProjId vars.SplitProjs = None then
                                allocated.Proj2Stud[projInt]
                            else

                                let len = vars.SplitProjs.[pref.ProjId].Length
                                let newStud = vars.SplitProjs.[pref.ProjId].[i]


                                if i = len - 1 then
                                    i <- 0
                                else
                                    i <- i + 1

                                newStud

                        else allocated.Proj2Stud[projInt]
                            

                    // rank of possible new move
                    let newStudRank = getProjRankFromPref data currAlloc.Stud (Some pref.ProjId)

                    // we eliminate current node
                    let isNotCurrProj = Some (pref.ProjId) <> currAlloc.Proj

                    // check if staff is loaded
                    let isNotLoaded = 
                        if stud = None then
                            (staffLoad.[supInt] < sup.MaxCap)
                        else
                            true

                    // check if student is approved for this project
                    let isApproved = 
                        if Map.tryFind (Pid projInt) sup.NoDefs <> None then
                            match sup.NoDefs.[Pid projInt] |> List.tryFind (fun student -> student = currStud) with
                            |Some x -> false
                            |None -> true 
                        else true

                    // check is new move rank is greater than restricted rank
                    let maxRank =
                        if input.MaxRankMoves = true then
                            if currAlloc.Depth = 0 then true
                            else newStudRank < input.RestrictAlloc
                        else
                            true
                    
                    // check if stud-proj pair is not blocked
                    let isNotBlocked = 
                        // case of self proposed
                        if input.BlockAlloc = true || input.SelfProposed then
                            //if input.DoubleAllocMaybes = true && vars.BlockMaybes = false && currAlloc.Depth = 0 then
                            //    printfn "blocked: %A" (vars.UnallocStuds |> Array.filter (fun el -> el = false)).Length

                            let valu = 
                                if stud <> None then
                                    vars.UnallocStuds.[sidToInt stud.Value] 
                                else true

                            valu && vars.UnallocStuds.[sidToInt vertex.Alloc.Stud.Value]
                            
                        // case of double allocation defs/maybes
                        elif input.DoubleAllocMaybes = true && 
                            vars.BlockMaybes = false && 
                            input.BlockSecondRound = true then
                                
                                let valu = 
                                    if stud <> None then
                                        vars.UnallocStuds.[sidToInt stud.Value] 
                                    else true

                                valu && vars.UnallocStuds.[sidToInt vertex.Alloc.Stud.Value]

                        // any other case is allowed
                        else true

                    // for firs round allocation when maybes are blocked 
                    // (only when double allocation maybes is activated)
                    let isNotBlockedMaybe = 
                        if input.DoubleAllocMaybes = true then

                            if vars.BlockMaybes = true then 
                                if Map.tryFind (Pid projInt) sup.Maybes <> None then
                                    match sup.Maybes.[Pid projInt] |> List.tryFind (fun student -> student = currStud) with
                                    |Some x -> false
                                    |None -> true 
                                else true

                            else 
                                true

                        else true

                    // every project has a limit of allocations
                    // default 1; split projects generally 2
                    let isBelowSplitLimit =
                        if input.SplitProjs = true then
                            if vars.MaxAllocs.[projInt] > 0 then
                                true
                            elif vars.MaxAllocs.[projInt] = 0 && stud <> None then
                                true
                            else 
                                false
                        else
                            true

                    
                    let ending = (now() - input.BreakTime) < 1000
                    
                    let filter = isNotCurrProj && isNotLoaded && isApproved && ending &&
                                    isNotBlockedMaybe && isNotBlocked &&
                                    maxRank && isBelowSplitLimit

                    //if checks = true then
                    //    let alloRank = getProjRankFromPref data stud (Some pref.ProjId)
                    //    printfn "alloRank: %A, %A" alloRank stud
                    //    printfn "notcurrProj, isnotLoaded, approved, isNotBlocked: %A, %A, %A, %A" isNotCurrProj  isNotLoaded  isApproved   isNotBlocked
                    //    printfn "filter : %A" filter

                    //if filter = true then
                    //    out.SpareCount <- out.SpareCount + 1

                    checks <- false

                    // if nodes are allowed than build new paths and calculate cost of new paths
                    match filter, stud with
                    |false, _ -> None
                    |true, Some sid ->

                        // prepare parameters
                        let studInt = sidToInt sid

                        // build new node
                        let newAlloc = 
                            {
                            Stud = Some sid
                            Proj = Some (pref.ProjId)
                            Depth = currAlloc.Depth + 1
                            }

                        let newNode = 
                            {
                                Alloc = newAlloc; 
                                Cost = movesPathCost data newAlloc currPath
                            }

                        let solPathCost =
                            match sol with
                            |Some x -> totalPathCost data vertex sol.Value.[0]
                            |None -> 100.0

                        // calculate costs of new path and stored path
                        let newPathCost = totalPathCost data vertex newNode
                        let storedPathCost = 
                            if allPaths.[studInt] <> List.empty then
                                totalPathCost data vertex allPaths.[studInt].[0]
                            else 100.0

                        // nodes are not allowed to repeat within the same path
                        // because that means a student would be deallocated twice
                        // (except when the node is a vertex node => rotation)
                        let findRepStud =
                                currPath 
                                |> List.filter (fun node -> node.Alloc.Stud <> vertex.Alloc.Stud)
                                |> List.tryFind (fun node ->
                                    node.Alloc.Stud = stud)

                        if (findRepStud = None) then
                            if newPathCost < storedPathCost then
                                allPaths.[studInt] <- newNode::currPath

                            //if checks = true then
                            //    printfn "newCost: %A"newPathCost

                            if newPathCost < solPathCost && newPathCost < 0.0 then
                                sol <- Some (newNode::currPath)
                                
                            Some newAlloc
                        else  
                            None

                    // same case as above but when the node does not have a student
                    |true, None -> 
                        let newAlloc = 
                            {
                            Stud = None
                            Proj = Some (pref.ProjId)
                            Depth = currAlloc.Depth + 1
                            }

                        let newNode = 
                            {
                            Alloc = newAlloc; 
                            Cost = movesPathCost data newAlloc currPath
                            }


                        let newPathCost = totalPathCost data vertex newNode
                        let solPathCost =
                            match sol with
                            |Some x -> totalPathCost data vertex sol.Value.[0]
                            |None -> 100.0


                        if newPathCost < solPathCost && newPathCost < 0.0 then
                            sol <- Some (newNode::currPath)

                        Some newAlloc
                        )

            let endTime = now()
           
            //let newCount = count + 1

            // filter in order to obtain only child nodes
            let newAllocs =
                newAllocsOpt
                |> List.filter (fun alloc -> alloc <> None)
                |> List.map (fun alloc -> alloc.Value)

            // for optimalPaths
            let emptyProjs =
                newAllocs
                |> List.filter (fun alloc -> alloc.Stud = None)
                |> List.map (fun alloc -> 
                    let newNode = 
                        {
                            Alloc = alloc; 
                            Cost = movesPathCost data alloc currPath
                        }
                    
                    newNode :: currPath
                    )

            // got the child nodes and added to queue
            let queueUpdated = 
                match queue with
                |h::lst  -> lst @ newAllocs 
                |_      -> failwithf "queue should not be empty"

            // for optimalPaths
            let newEnds = 
                match newAllocs with
                |h::lst -> endedPaths @ emptyProjs
                |[] -> currPath :: endedPaths @ emptyProjs

            // stop the algorithms if lasts for too long
            if (endTime - initialTime) < 100 then
                bfsModel2 data allocated mode currAlloc.Depth vertex visitedStuds
                            queueUpdated sol staffLoad newEnds allPaths
            else
                
                solution

        // case when nodes are not allowed to produce child nodes
        |_, true |2, _ -> 
            let queueUpdated = 
                match queue with
                |h::lst  -> lst
                |[]      -> []

            bfsModel2 data allocated mode currAlloc.Depth vertex visitedStuds
                        queueUpdated sol staffLoad endedPaths allPaths

        |_, _ -> failwithf "simpleBFS nodesConfig"
//-------------------------------------------------------------------------------------------



/// find all the possible possible allocations for a student by doing succesive shift moves
/// similar to a breadth first search algorithm for graphs (we consider data structure 
/// being similar to a graph)
/// possible allocations -> paths of the graph to an end node (project on a preference 
/// list not allocated to any student)
let rec bfsModel1 (data: StaticData) (allocated: AllocArrays)
                  (count: int) (vertex: Node)
                  (queue: Alloc list) (solution: Node list option) (staffLoad: int array)
                  (allPaths: Node list list array) =

    //printfn "------ ooooo -------- "
    if queue = List.empty then

        solution

    else
        // ---------- variables for categorising the bfs cases ---------
        //--------------------------------------------------------------
        let currAlloc = queue.[0]
        let mutable sol = solution

        if currAlloc.Depth = 0 then
            out.SpareCount <- 0
            out.SpareDepth <- 0

        

        let mutable checks = false


        let mutable i = 0

        // check if node is allowed to produce child nodes
        match nodesConfig currAlloc with
        |0 |1 -> // (s,p) ( ,p)
            //let currPath = allPaths.[sidToInt currAlloc.Stud.Value]
            let currStud = currAlloc.Stud.Value

            let initialTime = now()


            let newAllocsOpt = 
                data.Stud2Pref.[sidToInt currStud] // preferences of the current student
                |> List.map (fun pref ->
                    // for split projects which should be considered multiple times as
                    // they are allocated to multiple students

                    if Map.tryFind pref.ProjId vars.SplitProjs = None then
                        [pref]
                    else
                        List.replicate vars.SplitProjs.[pref.ProjId].Length pref)
                |> List.concat
                |> List.map (fun pref -> 
                    // -------------- FILTER CONDITIONS ---------------------
                    // we constrain moves and keep only good child nodes

                    // parameters we need
                    let projInt = pidToInt pref.ProjId
                    let supInt = stidToInt data.Proj2Sup.[projInt]
                    let sup = data.Staff.[supInt]

                    // find student of the child node (allocated student to the preference
                    // of parent node)
                    let stud = 
                        if input.SplitProjs = true then
                            if Map.tryFind pref.ProjId vars.SplitProjs = None then
                                allocated.Proj2Stud[projInt]
                            else

                                let len = vars.SplitProjs.[pref.ProjId].Length
                                let newStud = vars.SplitProjs.[pref.ProjId].[i]


                                if i = len - 1 then
                                    i <- 0
                                else
                                    i <- i + 1

                                newStud

                        else allocated.Proj2Stud[projInt]
                            

                    //if checks = true then
                    //    printfn "studs: %A" vars.SplitProjs.[Pid projInt]

                    // rank of possible new move
                    let newStudRank = getProjRankFromPref data currAlloc.Stud (Some pref.ProjId)

                    // we eliminate current node
                    let isNotCurrProj = Some (pref.ProjId) <> currAlloc.Proj

                    // check if staff is loaded
                    let isNotLoaded = 
                        if stud = None then
                            (staffLoad.[supInt] < sup.MaxCap)
                        else
                            true

                    // check if student is approved for this project
                    let isApproved = 
                        if Map.tryFind (Pid projInt) sup.NoDefs <> None then
                            match sup.NoDefs.[Pid projInt] |> List.tryFind (fun student -> student = currStud) with
                            |Some x -> false
                            |None -> true 
                        else true

                    // check is new move rank is greater than restricted rank
                    let maxRank =
                        if input.MaxRankMoves = true then
                            if currAlloc.Depth = 0 then true
                            else newStudRank < input.RestrictAlloc
                        else
                            true
                    
                    // check if stud-proj pair is not blocked
                    let isNotBlocked = 
                        // case of self proposed
                        if input.BlockAlloc = true || input.SelfProposed then
                            //if input.DoubleAllocMaybes = true && vars.BlockMaybes = false && currAlloc.Depth = 0 then
                            //    printfn "blocked: %A" (vars.UnallocStuds |> Array.filter (fun el -> el = false)).Length

                            let valu = 
                                if stud <> None then
                                    vars.UnallocStuds.[sidToInt stud.Value] 
                                else true

                            valu && vars.UnallocStuds.[sidToInt vertex.Alloc.Stud.Value]
                            
                        // case of double allocation defs/maybes
                        elif input.DoubleAllocMaybes = true && 
                            vars.BlockMaybes = false && 
                            input.BlockSecondRound = true then
                                
                                let valu = 
                                    if stud <> None then
                                        vars.UnallocStuds.[sidToInt stud.Value] 
                                    else true

                                valu && vars.UnallocStuds.[sidToInt vertex.Alloc.Stud.Value]

                        // any other case is allowed
                        else true

                    // for firs round allocation when maybes are blocked 
                    // (only when double allocation maybes is activated)
                    let isNotBlockedMaybe = 
                        if input.DoubleAllocMaybes = true then

                            if vars.BlockMaybes = true then 
                                if Map.tryFind (Pid projInt) sup.Maybes <> None then
                                    match sup.Maybes.[Pid projInt] |> List.tryFind (fun student -> student = currStud) with
                                    |Some x -> false
                                    |None -> true 
                                else true

                            else 
                                true

                        else true

                    // every project has a limit of allocations
                    // default 1; split projects generally 2
                    let isBelowSplitLimit =
                        if input.SplitProjs = true then
                            if vars.MaxAllocs.[projInt] > 0 then
                                true
                            elif vars.MaxAllocs.[projInt] = 0 && stud <> None then
                                true
                            else 
                                false
                        else
                            true
                            
                    let ending = (now() - input.BreakTime) < 10000
                    
                    let filter = isNotCurrProj && isNotLoaded && isApproved && ending &&
                                    isNotBlockedMaybe && isNotBlocked &&
                                    maxRank && isBelowSplitLimit

                    //if checks = true then
                    //    let alloRank = getProjRankFromPref data stud (Some pref.ProjId)
                    //    printfn "alloRank: %A, %A" alloRank stud
                    //    printfn "notcurrProj, isnotLoaded, approved, isNotBlocked: %A, %A, %A, %A" isNotCurrProj  isNotLoaded  isApproved   isNotBlocked
                    //    printfn "filter : %A" filter
                    //if filter = true then
                    //    out.SpareCount <- out.SpareCount + 1

                    // filter the nodes which are not allowed as child nodes
                    match filter, stud with
                    |false, _ -> None
                    |true, Some sid ->
                        //if checks = true then
                        //    printfn "heihei\n"
                        
                        // prepare parameters
                        let studInt = sidToInt sid
                        let newAlloc = 
                            {
                            Stud = Some sid
                            Proj = Some (pref.ProjId)
                            Depth = currAlloc.Depth + 1
                            }

                        // for all paths ending in parent node
                        let newPaths = 
                            allPaths.[sidToInt currAlloc.Stud.Value]
                            |> List.map (fun nodeLst -> 
                                
                                //if now() - initialTime > 100 && checks = true then
                                //    printfn "step 1"

                                if now() - initialTime < 100 then
                                    let findRepStud =
                                            nodeLst 
                                            |> List.tryFind (fun node ->
                                                node.Alloc.Stud = newAlloc.Stud)

                                    if (findRepStud = None || findRepStud.Value.Alloc.Stud = vertex.Alloc.Stud) then
                                        let newNode = 
                                            {
                                                Alloc = newAlloc; 
                                                Cost = movesPathCost data newAlloc nodeLst
                                            }

                                        let newPathCost = totalPathCost data vertex newNode
                                        let solPathCost =
                                            match sol with
                                            |Some x -> totalPathCost data vertex sol.Value.[0]
                                            |None -> 100.0
                                        
                                        //if currAlloc.Depth = 0 then
                                        //    printfn "prevCost: %A" solPathCost
                                        //    printfn "newCost: %A" newPathCost

                                        // if cost of new path smaller than previous then store it
                                        if newPathCost < solPathCost  then
                                            if newPathCost < 0.0 then
                                                
                                                //if checks = true then
                                                //    printfn "prevCost: %A" solPathCost
                                                //    printfn "newCost: %A" newPathCost
                                                //    printfn "sol %A" sol
                                                sol <- Some (newNode::nodeLst)
                                            else 
                                                if newPathCost = 0.0 && 
                                                    data.Stud2Pref.[sidToInt sid].Length < 5 &&
                                                    data.Stud2Pref.[sidToInt vertex.Alloc.Stud.Value].Length >= 4 then
                                                
                                                    sol <- Some (newNode::nodeLst)
                                        newNode::nodeLst
                                
                                    else
                                        []
                                else 
                                    []
                                )
                            |> List.filter (fun lst -> lst <> List.empty)

                        // store new paths built and save new child nodes
                        allPaths.[studInt] <- newPaths @ allPaths.[studInt]
                        Some newAlloc

                    |true, None -> //( ,p)
                        //if checks = true then
                        //    printfn "heiheihei\n"
                        
                        let newAlloc = 
                            {
                            Stud = None
                            Proj = Some (pref.ProjId)
                            Depth = currAlloc.Depth + 1
                            }

                            
                        allPaths.[sidToInt currAlloc.Stud.Value]
                        |> List.iter (fun nodeLst -> 
                            
                            //if now() - initialTime > 100 && checks = true then
                            //        printfn "step 2"
                            
                            let newNode = 
                                {
                                Alloc = newAlloc; 
                                Cost = movesPathCost data newAlloc nodeLst
                                }
                            let newPathCost = totalPathCost data vertex newNode

                            let solPathCost =
                                match sol with
                                |Some x -> totalPathCost data vertex sol.Value.[0]
                                |None -> 100.0

                            //if currAlloc.Depth = 0 then
                            //        printfn "prevCost: %A" solPathCost
                            //        printfn "newCost: %A" newPathCost

                            if newPathCost < solPathCost && newPathCost < 0.0 then
                                sol <- Some (newNode::nodeLst)
                                //if checks = true then
                                //    printfn "prevCost: %A" solPathCost
                                //    printfn "newCost: %A" newPathCost
                                //    printfn "sol %A" sol
                                
                                )
                                

                        Some newAlloc)

            let endTime = now()

            //let newCount = count + 1


            // obtain the child nodes and add them to queue
            let newAllocs =
                newAllocsOpt
                |> List.filter (fun alloc -> alloc <> None)
                |> List.map (fun alloc -> alloc.Value)

            let queueUpdated = 
                match queue with
                |h::lst  -> lst @ newAllocs
                |_      -> failwithf "queue should not be empty"


            if (endTime - initialTime) < 100 then
                
                bfsModel1 data allocated (count + 1) vertex
                            queueUpdated sol staffLoad allPaths 
            else
                out.SpareDepth <- currAlloc.Depth
                //if solution <> None then
                    //printfn "solution cost: %A" (totalPathCost data vertex sol.Value.[0])
                sol
            

        |2 -> 
            let queueUpdated = 
                match queue with
                |h::lst  -> lst
                |[]      -> []

            //printfn" here 2 "

            bfsModel1 data allocated (count+1) vertex
                        queueUpdated sol staffLoad allPaths 

        |_ -> failwithf "simpleBFS nodesConfig"
//-------------------------------------------------------------------------------------------


/// once a solution is returned, we update the allocation arrays and other
/// relevant collection types for split projects and maximum allowed allocations
/// per project
let shiftProjs (data: StaticData) (allocated: AllocArrays) (movesLst: Node list)
                (staffLoad: int array) =

    // updates all the permutations except student vertex and leaf project
    movesLst 
    |> List.pairwise
    |> List.iteri 
        (fun i nodePair -> 
            let studInt = sidToInt (snd nodePair).Alloc.Stud.Value
            let projInt = pidToInt (fst nodePair).Alloc.Proj.Value
            allocated.Stud2Proj.[studInt] <- Some (Pid projInt)
            allocated.Proj2Stud.[projInt] <- Some (Sid studInt)

            if input.SplitProjs = true then
                let studLeaf = (fst nodePair).Alloc.Stud

                 

                if Map.tryFind (Pid projInt) vars.SplitProjs <> None then

                    //printfn "allocations: %A\n" vars.SplitProjs.[Pid projInt]
                    let emptySlot = 
                        vars.SplitProjs.[Pid projInt]
                        |> Array.findIndex (fun student -> student = studLeaf)

                    vars.SplitProjs.[Pid projInt].[emptySlot] <- Some (Sid studInt)
                    
        )

    let vertex = List.last movesLst

    let pair = possibPairsNodes vertex.Alloc movesLst.[0].Alloc

    // updates student vertex and leaf project depending on case
    if vertex.Alloc.Stud <> movesLst.[0].Alloc.Stud then
        if pair = 0 then // (s/p) -> (s/p)
            let projVertex = pidToInt vertex.Alloc.Proj.Value
            let studVertex = vertex.Alloc.Stud
            let supVertex = stidToInt data.Proj2Sup[projVertex]

            let studLeaf = sidToInt movesLst.[0].Alloc.Stud.Value

            vars.MaxAllocs.[projVertex] <- vars.MaxAllocs.[projVertex] + 1

            if input.SplitProjs = true then
                if Map.tryFind (Pid projVertex) vars.SplitProjs <> None then
                    //printfn "pair = 0"
                    //printfn "studLeaf: %A, proj %A" studVertex projVertex 
                    //printfn "allocations: %A\n" constraintsProjs.[Pid projVertex]
                    let emptySlot = 
                        vars.SplitProjs.[Pid projVertex]
                        |> Array.tryFindIndex (fun student -> student = studVertex)


                    vars.SplitProjs.[Pid projVertex].[emptySlot.Value] <- None
                    //constraintsProjs
                    //|> Map.iter (fun key studs -> printfn "proj: %A, studs: %A" key studs)

                
                    

            allocated.Proj2Stud.[projVertex] <- None
            allocated.Stud2Proj.[studLeaf] <- None
            staffLoad.[supVertex] <- staffLoad.[supVertex] - 1
    
        elif pair = 1 then // (s/p) -> ( /p)
            let projVertex = pidToInt vertex.Alloc.Proj.Value
            let supVertex = stidToInt data.Proj2Sup[projVertex]
            let studVertex = vertex.Alloc.Stud

            let projLeaf = pidToInt movesLst.[0].Alloc.Proj.Value
            let supLeaf = stidToInt data.Proj2Sup[projLeaf]

            vars.MaxAllocs.[projVertex] <- vars.MaxAllocs.[projVertex] + 1
            vars.MaxAllocs.[projLeaf] <- vars.MaxAllocs.[projLeaf] - 1

            if input.SplitProjs = true then
                if Map.tryFind (Pid projVertex) vars.SplitProjs <> None then

                    //printfn "pair = 1"
                    //printfn "studLeaf: %A, proj %A" studVertex projVertex 
                    //printfn "allocations: %A\n" constraintsProjs.[Pid projVertex]

                    let emptySlot = 
                        vars.SplitProjs.[Pid projVertex]
                        |> Array.tryFindIndex (fun student -> student = studVertex)
                
                    vars.SplitProjs.[Pid projVertex].[emptySlot.Value] <- None

                    //constraintsProjs
                    //|> Map.iter (fun key studs -> printfn "proj: %A, studs: %A" key studs)

            allocated.Proj2Stud.[projVertex] <- None
            staffLoad.[supVertex] <- staffLoad.[supVertex] - 1
            staffLoad.[supLeaf] <- staffLoad.[supLeaf] + 1
        
        elif pair = 2 then // (s/ ) -> (s/p)

            let studLeaf = sidToInt movesLst.[0].Alloc.Stud.Value

            allocated.Stud2Proj.[studLeaf] <- None

        elif pair = 3 then // (s/ ) -> ( /p)
            let projLeaf = pidToInt movesLst.[0].Alloc.Proj.Value
            let supLeaf = stidToInt data.Proj2Sup[projLeaf]

            vars.MaxAllocs.[projLeaf] <- vars.MaxAllocs.[projLeaf] - 1

            staffLoad.[supLeaf] <- staffLoad.[supLeaf] + 1


    //allocated.Stud2Proj
    //|> Array.iteri (fun i proj ->
    //    //printfn "proj %A, %A" i proj
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
    //                allocated.Stud2Proj
    //                |> Array.removeAt i
    //                |> Array.tryFind (fun pid -> 
    //                    //printfn "stud %A, %A" i proj
    //                    pid = proj)
    //            if result <> None then
    //                allocated.Stud2Proj
    //                |> Array.iteri (fun i proj ->
    //                    printfn "stud %A, %A" i proj)
    //                failwithf "%A\n, %A\n" proj movesLst)

    // checks if student is allocated twice => failure
    allocated.Proj2Stud
    |> Array.iteri (fun i stud ->
        //printfn "proj %A, %A" i stud
        if stud <> None then
            let result =
                allocated.Proj2Stud
                |> Array.removeAt i
                |> Array.tryFind (fun sid -> 
                    sid = stud)
            if result <> None then
                allocated.Proj2Stud
                |> Array.iteri (fun i proj ->
                    printfn "proj %A, %A" i proj)
                failwithf "%A\n, %A\n" stud movesLst)

    movesLst.[0].Alloc.Stud

    


// we take a student at a time and apply the breadth first search algorithm
let rec tryAllocStudent (data: StaticData) (allocated: AllocArrays)
                      (staffLoad: int array) (queueStuds: StudentId list): AllocArrays =

    //printfn "---------------------------------------------------------------------------"

    match queueStuds with
    |[]     -> 
        allocated

    |lst    -> 
        let currStud = queueStuds.[0]


        // try to allocate the selected student (do bfs)
        let queueNodes = [{Stud = Some(currStud); Proj = allocated.Stud2Proj.[sidToInt currStud]; Depth = 0}]
        let vertex = {Alloc = queueNodes.[0]; Cost = {Shift = 0; Seq = 0}}
        let solution = None

        //printfn "----------"
        //printfn "currStud: %A\n" currStud

        input.BreakTime <- now()

        let newAllocs = 
            if input.isModelOne = true then
                let allPaths: Node list list array = Array.create data.NumStudents []
                allPaths.[sidToInt currStud] <- [[{Alloc = queueNodes.[0];
                                   Cost = {Shift = 0.0; Seq = 0.0}}]]

                (bfsModel1 data allocated 0 vertex queueNodes solution
                            staffLoad allPaths)
            else 
                let allPaths: Node list array = Array.create data.NumStudents []
                allPaths.[sidToInt currStud] <- [{Alloc = queueNodes.[0];
                                   Cost = {Shift = 0.0; Seq = 0.0}}]
        
                let visitedStuds = Array.create data.NumStudents false

                (bfsModel2 data allocated 0 0 vertex visitedStuds queueNodes solution
                            staffLoad [] allPaths)

        //printfn "newAllocs: %A" newAllocs

        let queueStudsUpt = 
            match queueStuds with
            | []    -> []
            | h::t  -> t

        if newAllocs <> None then
            shiftProjs data allocated newAllocs.Value staffLoad |> ignore
            //let leafStud = newAllocs.Value.[0].Alloc.Stud
            //let vertexProj = vertex.Alloc.Proj
            

        // just for statistics purposes, no influence in allocation-------------
            out.Counts <- out.Counts + out.SpareCount
            out.SumDepth <- out.SumDepth + out.SpareDepth
                
            
        if out.MaxDepth < out.SpareDepth then
            out.MaxDepth <- out.SpareDepth
        
        if out.MaxCount < out.SpareCount then
            out.MaxCount <- out.SpareCount
        //-----------------------------------------------------

        
        tryAllocStudent data allocated staffLoad queueStudsUpt 




// creates a queue of students and starts the process of allocation
let allocStudents (data : StaticData) (allocated: AllocArrays) (staffLoad: int array) : (AllocArrays) =
    //let staffCap = data.Proj2Staff |> Array.map (fun staff -> staff.MaxCap)

    let queueStuds =
        if input.RandomOrder then
            genRandomNumbers data.NumStudents data.NumStudents
            |> List.map (fun el -> Sid el) 
        else 
            
            data.AllocPens
            |> Array.toList
            |> List.mapi (fun i pen -> (i, pen))
            |> List.sortByDescending (fun pair -> snd pair)
            |> List.map (fun pair -> Sid (fst pair))

    printfn "Defs\n"

    tryAllocStudent data allocated staffLoad queueStuds 


