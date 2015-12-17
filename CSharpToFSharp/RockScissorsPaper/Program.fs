(*
    This code accompanies a blog post at https://csharptofsharp.wordpress.com/2015/12/10/what-came-before-today/

    It is not good F#.

    It is the first console app I wrote in F#, with me coming from the perspective of C# and imperative language.
*)
type choice = Rock | Scissors | Paper
type result = First | Tie | Second

let choices = [Rock; Scissors; Paper]

let combinations = [
    ((Rock, Rock), Tie);
    ((Scissors, Scissors), Tie);
    ((Paper, Paper), Tie);
    ((Rock, Scissors), First);
    ((Scissors, Paper), First);
    ((Paper, Rock), First);
    ((Rock, Paper), Second);
    ((Scissors, Rock), Second);
    ((Paper, Scissors), Second)]

type choiceDescriptor = {
    Choice: choice;
    Key: System.ConsoleKey;
    Description: string;
}

let choiceDescriptors = [{Choice= Rock; Key= System.ConsoleKey.R; Description="Rock"};
                        {Choice= Scissors; Key= System.ConsoleKey.S; Description="Scissors"};
                        {Choice= Paper; Key= System.ConsoleKey.P; Description="Paper"}]

let choiceName choice=
    let choiceDescriptor = 
        choiceDescriptors|>List.find(fun i->i.Choice=choice)
    choiceDescriptor.Description

let validateInput key=
    let choiceDescriptor = 
        choiceDescriptors|>List.tryFind(fun i->i.Key=key)
    if choiceDescriptor.IsSome 
    then Some choiceDescriptor.Value.Choice
    else None

type resultDescriptor = {
    Result: result;
    Message: string;}

let resultDescriptors = [{Result=First; Message="Player wins round!"};
                        {Result=Second; Message="Computer wins round!"};
                        {Result=Tie; Message="Tie!"}]

let rec readPlayerChoice =
    fun()->
    let keyInfo = System.Console.ReadKey(true).Key
    let input = 
        validateInput keyInfo
    if input.IsSome 
    then input.Value 
    else readPlayerChoice()

let randomChoice = fun()->
    choices|>List.sortBy(fun x->System.Guid.NewGuid())|>List.head

let resultMessage x =
    let resultDescriptor = 
        resultDescriptors|>List.find(fun i->i.Result=x)
    resultDescriptor.Message

let determineResult first second =
    let _,result= combinations|> List.find(fun item->
        let combination,_=item
        combination=(first,second))
    result

let playRound = fun()->
    do System.Console.WriteLine("Choose [R]ock, [S]cissors, [P]aper:");
    let playerChoice = readPlayerChoice()
    do System.Console.WriteLine("Player Choice: {0}", choiceName playerChoice)
    let computerChoice = randomChoice()
    do System.Console.WriteLine("Computer Choice: {0}", choiceName computerChoice)
    let roundResult = determineResult playerChoice computerChoice
    do System.Console.WriteLine(resultMessage roundResult)
    roundResult

type game = {
    round: int;
    playerScore: int;
    computerScore: int;
    winsPerGame: int;
}

let rec playGame game=
    if game.computerScore>=game.winsPerGame then
        do System.Console.WriteLine("Computer wins!")
        game
    elif game.playerScore>=game.winsPerGame then
        do System.Console.WriteLine("Player wins!")
        game
    else 
        do System.Console.WriteLine("Round {0}",game.round);
        do System.Console.WriteLine("Player Score: {0}", game.playerScore);
        do System.Console.WriteLine("Computer Score: {0}", game.computerScore);
        let roundResult = playRound()
        match roundResult with
        | x when x=First -> 
            let newGame= { round= game.round+1; playerScore=game.playerScore+1; computerScore=game.computerScore; winsPerGame=game.winsPerGame}
            playGame newGame
        | x when x=Second ->
            let newGame= { round= game.round+1; playerScore=game.playerScore; computerScore=game.computerScore+1; winsPerGame=game.winsPerGame}
            playGame newGame
        | _ -> 
            let newGame= { round= game.round+1; playerScore=game.playerScore; computerScore=game.computerScore; winsPerGame=game.winsPerGame}
            playGame newGame

[<EntryPoint>]
let main argv = 
    let newGame = {round=1; playerScore=0; computerScore=0; winsPerGame=3}
    playGame newGame|>ignore
    do System.Console.ReadLine()|>ignore
    0