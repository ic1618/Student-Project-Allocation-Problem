module Database

open AllocTypes

// ------------ TEST 1 -----------
let database1 = {
    Stud2Pref = [| 
            [   // Sid 0
                {ProjId = Pid 3; Rank = 1.0}
            ];
                // Sid 1, 
            [
                {ProjId = Pid 0; Rank = 2.0}
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 3; Rank = 3.0}
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 5; Rank = 5.0}
            ];
                // Sid 2, 
            [
                {ProjId = Pid 4; Rank = 3.0}
                {ProjId = Pid 5; Rank = 2.0}
                {ProjId = Pid 7; Rank = 3.0}
                {ProjId = Pid 6; Rank = 4.0}
                {ProjId = Pid 0; Rank = 5.0}
            ];
                // Sid 3, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 4; Rank = 2.0}
            ];
                // Sid 4, 
            [
                {ProjId = Pid 6; Rank = 1.0}
                {ProjId = Pid 8; Rank = 2.0}

            ];
                // Sid 5, 
            [
                {ProjId = Pid 0; Rank = 1.0}
                {ProjId = Pid 4; Rank = 2.0}
                {ProjId = Pid 7; Rank = 2.0}
            ];
                // Sid 6, 
            [
                {ProjId = Pid 8; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
            ];
                // Sid 7, 
            [
                {ProjId = Pid 6; Rank = 1.0}
                {ProjId = Pid 8; Rank = 3.0}
            ]|];
    Proj2Sup = [|Stid 8; Stid 7; Stid 6; Stid 5; Stid 4; Stid 3; Stid 2; Stid 1; Stid 0 |];
    Staff = [|
                    {   MaxCap = 20; 
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map []
                    }
                 |];
    AllocPens = [|10; 10; 10; 10; 10; 10; 10; 10|]
    NumStudents = 8;
    NumProjects = 9;
    NumSups = 9
}

let allocated1 = {
                //     0            1           2           3           4           5           6           7             8
    Stud2Proj = [|Some(Pid 3); Some(Pid 0); Some(Pid 4); Some(Pid 1); Some(Pid 6); Some(Pid 7); Some(Pid 2); Some(Pid 8)|];
    Proj2Stud = [|Some(Sid 1); Some(Sid 3); Some(Sid 6); Some(Sid 0); Some(Sid 2);        None; Some(Sid 4);        None; Some(Sid 7)|]
}

let visitedStuds1 = Array.create database1.NumStudents false
let queue1: Alloc list = [ {Stud = Some(Sid 5); Proj = None; Depth = 0} ]
let solution1: Node list option = None
let staffCap1: int array = Array.create database1.NumProjects 0
let allPaths1: Node list list array = Array.create database1.NumProjects []
allPaths1.[5] <- [[{Alloc = {Stud = Some(Sid 5); Proj = None; Depth = 0};
                           Cost = {Shift = 0.0; Seq = 0.0}}]]
let vertex1 = {Alloc = {Stud = Some(Sid 5); Proj = None; Depth = 0};
                           Cost = {Shift = 0.0; Seq = 0.0}}


////let visitedProjs1: int array = Array.create database1.NumProjects 0
//let emptyProjs1: Alloc list = []
//let potentialSols1: Alloc list array = Array.create 7 []
//let rotations1: Node list list array = Array.create 1 []
//let endedPaths1: Node list list = []
//let allPaths1: Node list array = Array.create database1.NumProjects []
////let possibPaths1: Node list array = Array.create database1.NumProjects []


// -------------------------------------------------------
let database2 = {
    Stud2Pref = [|
                //Sid 0, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
                //{ProjId = Pid 5; Rank = 3.0}

            ]
                //Sid 1, 
            [
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 3; Rank = 2.0}
                {ProjId = Pid 1; Rank = 3.0}

            ]
                //Sid 2, 
            [
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
            ]
                //Sid 3, 
            [
                {ProjId = Pid 3; Rank = 1.0}
                {ProjId = Pid 4; Rank = 2.0}
            ]
                //Sid 4, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 6; Rank = 2.0}
                {ProjId = Pid 4; Rank = 3.0}
            ]
    |];
    Proj2Sup = [|Stid 0; Stid 1; Stid 2; Stid 3|];
    Staff = [|
                    {   
                        MaxCap = 20; 
                        Maybes = Map []
                        NoDefs = Map [ (Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]); 
                                    (Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3])]
                    }
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map [ (Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]); 
                                    (Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3])]}
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map [ (Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]); 
                                    (Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3])]}
                    {   
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map [ (Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]); 
                                    (Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3])]}
                    {    
                        MaxCap = 20;
                        Maybes = Map []
                        NoDefs = Map [ (Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]); 
                                    (Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3])]}
                 |];

    AllocPens = Array.create 5 10
    NumStudents = 5;
    NumProjects = 7;
    NumSups = 2
}

let allocated2 = {
                //     0            1           2           3           4           5           6           7             8
    Stud2Proj = [|       None; Some(Pid 1); Some(Pid 2); Some(Pid 3); Some(Pid 4)|];
    Proj2Stud = [|       None; Some(Sid 1); Some(Sid 2); Some(Sid 3); Some(Sid 4);  None;       None|]
}

let queue2: Alloc list = [ {Stud = Some(Sid 0); Proj = None; Depth = 0} ]
let vertex2 = { Alloc = queue2.[0]; Cost = {Shift = 0.0; Seq = 0.0}}
let solution2: Node list = [{Alloc = {Stud = None; Proj = None; Depth = 0};
                             Cost = {Shift = 0.0; Seq = 0.0}}]
let staffCap2: int array = Array.create database2.NumProjects 0
let allPaths2: Node list list array = Array.create database2.NumStudents []
allPaths2.[0] <- [[{Alloc = {Stud = Some(Sid 0); Proj = None; Depth = 0};
                           Cost = {Shift = 0.0; Seq = 0.0}}]]

//-----------------------------------------------------------------------------

// -----------------
//------ DB 3 ------
//------------------

let database3 = {
    Stud2Pref = [|
                //Sid 0, 
            [
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
                {ProjId = Pid 0; Rank = 3.0}

            ]
                // Sid 1, 
            [
                {ProjId = Pid 2; Rank = 1.0}
                {ProjId = Pid 4; Rank = 2.0}
                {ProjId = Pid 3; Rank = 3.0}
            
            ]
                //Sid 2, 
            [
                {ProjId = Pid 3; Rank = 1.0}
                {ProjId = Pid 1; Rank = 2.0}
                {ProjId = Pid 2; Rank = 3.0}

            ]
                //Sid 3, 
            [
                {ProjId = Pid 2; Rank = 1.0}
                {ProjId = Pid 0; Rank = 2.0}
                {ProjId = Pid 3; Rank = 3.0}
            ]
                |];
            // Pids  0      1       2       3       4
    Proj2Sup = [|Stid 0; Stid 1; Stid 1; Stid 1; Stid 0 |]
    Staff = [|          // Stid 0
                    {    
                        MaxCap = 1; 
                        Maybes = Map []
                        NoDefs = Map [(Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 4, [Sid 1; Sid 2; Sid 3])]
                    }
                    {    //Stid 1
                        MaxCap = 2; 
                        Maybes = Map []
                        NoDefs = Map [(Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3])]
                    }
                 |]
    AllocPens = Array.create 4 10
    NumStudents = 4;
    NumProjects = 5;
    NumSups = 2;
}
let allocated3 = {
    Stud2Proj = Array.create 4 None;
    Proj2Stud = Array.create 5 None;
}
//---------------------------------------------------------------------


//----------------
//----- DB 4 -----
//----------------

let database4 = {
    Stud2Pref = [|
                //Sid 0, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
            ]
                // Sid 1, 
            [
                {ProjId = Pid 5; Rank = 1.0}
                {ProjId = Pid 0; Rank = 2.0}
                {ProjId = Pid 2; Rank = 3.0}
                {ProjId = Pid 1; Rank = 4.0}
            ] 
                //Sid 2, 
            [
                {ProjId = Pid 5; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
                {ProjId = Pid 6; Rank = 3.0}

            ]
                //Sid 3, 
            [
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
            ]
                //Sid 4, 
            [
                {ProjId = Pid 3; Rank = 1.0}
                {ProjId = Pid 0; Rank = 2.0}
                {ProjId = Pid 2; Rank = 3.0}
                {ProjId = Pid 4; Rank = 4.0}
            ]
                //Sid 5, 
            [
                {ProjId = Pid 0; Rank = 1.0}
                {ProjId = Pid 4; Rank = 2.0}
            ]
                |];
            // Pids  0      1       2       3       4       5       6
    Proj2Sup = [|Stid 0; Stid 2; Stid 1; Stid 1; Stid 0; Stid 2; Stid 0 |]
    Staff = [|          // Stid 0
                    {   
                        MaxCap = 3; 
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   // Stid 1
                        MaxCap = 1; 
                        Maybes = Map []
                        NoDefs = Map []
                    }
                    {   // Stid 2
                        MaxCap = 2; 
                        Maybes = Map []
                        NoDefs = Map [(Pid 1, [Sid 0])]
                    }
                 |]
    AllocPens = Array.create 6 10
    NumStudents = 6;
    NumProjects = 7;
    NumSups = 3;
}
let allocated4 = {
    Stud2Proj = Array.create 6 None;
    Proj2Stud = Array.create 7 None;
}
//-------------------------------------------------------------------


//let paths = genStudsPrefs 10 10 2 4
//let mutable i = 0
//for path in paths do
//    printfn "stud %A: %A" i path
//    i <- i+1

//printfn ""

//genRandomNumbers 10 10


//----------------
//----- DB 5 -----
//----------------

let database5 = {
    Stud2Pref = [|
                //Sid 0, 
            [
                {ProjId = Pid 0; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
                {ProjId = Pid 4; Rank = 3.0}
                {ProjId = Pid 6; Rank = 4.0}
            ]
                // Sid 1, 
            [
                {ProjId = Pid 7; Rank = 1.0}
                {ProjId = Pid 5; Rank = 2.0}
                {ProjId = Pid 4; Rank = 3.0}
                {ProjId = Pid 2; Rank = 4.0}
            ] 
                //Sid 2, 
            [
                {ProjId = Pid 0; Rank = 1.0}
                {ProjId = Pid 5; Rank = 2.0}
            ]
                //Sid 3, 
            [
                {ProjId = Pid 4; Rank = 1.0}
                {ProjId = Pid 6; Rank = 2.0}
                {ProjId = Pid 2; Rank = 3.0}
                {ProjId = Pid 5; Rank = 4.0}
            ]
                //Sid 4, 
            [
                {ProjId = Pid 2; Rank = 1.0}
                {ProjId = Pid 0; Rank = 2.0}
                {ProjId = Pid 7; Rank = 3.0}
            ]
                //Sid 5, 
            [
                {ProjId = Pid 0; Rank = 1.0}
                {ProjId = Pid 2; Rank = 2.0}
                {ProjId = Pid 4; Rank = 2.0}
                {ProjId = Pid 7; Rank = 2.0}
            ]
                //Sid 6, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 8; Rank = 2.0}
                {ProjId = Pid 9; Rank = 3.0}
            ]
                //Sid 7, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 0; Rank = 2.0}
            ]
                //Sid 8, 
            [
                {ProjId = Pid 1; Rank = 1.0}
                {ProjId = Pid 7; Rank = 2.0}
                {ProjId = Pid 5; Rank = 3.0}
                {ProjId = Pid 2; Rank = 4.0}
            ]
                //Sid 9, 
            [
                {ProjId = Pid 2; Rank = 1.0}
                {ProjId = Pid 1; Rank = 2.0}
                {ProjId = Pid 6; Rank = 3.0}
                {ProjId = Pid 3; Rank = 4.0}
            ]

                |];
            // Pids  0      1       2       3       4       5       6       7       8       9
    Proj2Sup = [|Stid 0; Stid 2; Stid 1; Stid 1; Stid 0; Stid 2; Stid 0; Stid 1; Stid 1; Stid 2|]
    Staff = [|          // Stid 0
                    {   
                        MaxCap = 3; 
                        Maybes = Map []
                        NoDefs = Map [(Pid 0, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 4, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 6, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9])]
                    }
                    {   // Stid 1
                        MaxCap = 4; 
                        Maybes = Map[]
                        NoDefs = Map [(Pid 2, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 3, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 7, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 8, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9])]
                    }
                    {   // Stid 2
                        MaxCap = 3; 
                        Maybes = Map[]
                        NoDefs = Map [(Pid 1, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 5, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9]);
                                    (Pid 9, [Sid 0; Sid 1; Sid 2; Sid 3; Sid 4; Sid 5; Sid 6; Sid 7; Sid 8; Sid 9])]
                    }
                 |]
    AllocPens = Array.create 10 10
    NumStudents = 10;
    NumProjects = 10;
    NumSups = 3;
}
let allocated5 = {
    Stud2Proj = Array.create 10 None;
    Proj2Stud = Array.create 10 None;
}
//-------------------------------------------------------------------
