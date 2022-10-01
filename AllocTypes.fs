module AllocTypes

//---------------Define Types--------------
type StudentId = Sid of int

type ProjectId = Pid of int

type StaffId = Stid of int

//------------Define Data Strucs-----------

// parameters needed throughout allocation
type Parameters = {
    UnallocStuds: bool array
    mutable BlockMaybes: bool
    MaxAllocs: int array
    SplitProjs: Map<ProjectId, StudentId option array>
}

// input data
type InputData = {
    mutable BreakTime: int64
    StopTime: int
    DefaultLoad: int
    PenaltyWeights: (float * float * float)
    MaybeCost: float
    RestrictAlloc: float
    
    isModelOne: bool
    oneRun: bool
    
    RandomOrder: bool
    PenaltyMaybes: bool
    DoubleAllocMaybes: bool
    BlockSecondRound: bool

    MaxRankMoves: bool
    BlockAlloc: bool
    NonlinearPenalty: bool
    SelfProposed: bool
    SplitProjs: bool

    //
    //FstPrefsAlloc: bool 
}

type OutputData = {
    mutable SpareDepth: int
    mutable MaxDepth: int
    mutable SumDepth: int
    mutable SpareCount: int
    mutable MaxCount: int
    mutable Counts: int
    mutable StaffLoad: int array
}

//type OutputData =

// student will contain a list of type Preference
type Preference ={
    ProjId: ProjectId
    Rank: float
}

// supervisor contains max cap list of maybes and no definites
type Supervisor = {
    MaxCap: int
    Maybes: Map<ProjectId, StudentId list>
    NoDefs: Map<ProjectId, StudentId list> 
}


// allocation defined below
type Alloc = {
    Stud: StudentId option
    Proj: ProjectId option
    Depth: int
}

// cost defined below
type Cost = {
    Shift: float
    Seq: float 
}

// node contains an allocation pair (stud - proj)
// cost for the move with respect to the previous node
// and cost for all path until that move
type Node = {
    Alloc: Alloc
    Cost: Cost
}

// all the data we need about the dataset
// student preferences
// project index -> supervisor ID
// supervisor index -> lists of maybes and No definites
// allocation penalty array for every student
// number of students
// number of projects
// number of supervisors
type StaticData = {
    Stud2Pref: Preference list array
    Proj2Sup: StaffId array
    Staff: Supervisor array
    AllocPens: float array
    NumStudents: int
    NumProjects: int
    NumSups: int
}


// Stud2Proj - array with student index that contains the project ID allocated
// Stud2Proj - array with project index that contains the student ID allocated
type AllocArrays = { 
    Stud2Proj : ProjectId option array
    Proj2Stud : StudentId option array
}


/// functions to get the int value of a StudentId or ProjectId
let pidToInt = function Pid x -> x
let sidToInt = function Sid x -> x
let stidToInt = function Stid x -> x