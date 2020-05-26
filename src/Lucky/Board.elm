module Lucky.Board exposing
    ( Answer(..)
    , Answered(..)
    , Board
    , FinalSelection(..)
    , Location(..)
    , Question(..)
    , Selection(..)
    , Tile(..)
    , answer
    , clearFinalAnswer
    , finalAnswer
    , loading
    , map
    , new
    , question
    , selected
    )


type Answer
    = Pass
    | Fail


type Answered
    = AnswerAt Location Answer


type Question
    = Single
    | Team


type Location
    = Top
    | Middle
    | Bottom


type Selection
    = NotSelected
    | LoadingAt Location
    | QuestionAt Location Question


type FinalSelection
    = FinalFuture
    | FinalQuestion
    | FinalAnswer Answer


type Tile
    = Answer Answered
    | Selection Selection
    | Future
    | Final FinalSelection


type Board
    = Board
        { maxSize : Int
        , answers : List Answered
        , current : Selection
        , final : FinalSelection
        }


new : Board
new =
    Board { maxSize = 9, answers = [], current = NotSelected, final = FinalQuestion }


selected : Board -> Tile
selected ((Board board) as original) =
    if remaining original < 0 then
        Final board.final

    else
        Selection board.current


loading : Location -> Board -> Board
loading location original =
    select (LoadingAt location) original


question : Question -> Board -> Board
question question_ ((Board board) as original) =
    case board.current of
        NotSelected ->
            original

        LoadingAt location ->
            select (QuestionAt location question_) original

        QuestionAt location _ ->
            select (QuestionAt location question_) original


select : Selection -> Board -> Board
select selection ((Board board) as original) =
    if remaining original < 0 then
        original

    else
        Board { board | current = selection }


answer : Answer -> Board -> Board
answer answer_ ((Board board) as original) =
    case board.current of
        NotSelected ->
            original

        LoadingAt location ->
            answerAt location answer_ original

        QuestionAt location _ ->
            answerAt location answer_ original


clearFinalAnswer : Board -> Board
clearFinalAnswer (Board board) =
    Board { board | final = FinalQuestion }


finalAnswer : Answer -> Board -> Board
finalAnswer answer_ ((Board board) as original) =
    if remaining original < 0 then
        Board { board | final = FinalAnswer answer_ }

    else
        original


answerAt : Location -> Answer -> Board -> Board
answerAt location answer_ (Board board) =
    Board
        { board
            | current = NotSelected
            , answers = AnswerAt location answer_ :: board.answers
        }


map : (Tile -> a) -> Board -> List a
map func original =
    mapAnswers (Answer >> func) original
        ++ mapSelected (Selection >> func) original
        ++ mapRemaining ((\_ -> Future) >> func) original
        ++ mapFinal (Final >> func) original


mapSelected : (Selection -> a) -> Board -> List a
mapSelected func ((Board board) as original) =
    if remaining original < 0 then
        []

    else
        [ func board.current ]


mapAnswers : (Answered -> a) -> Board -> List a
mapAnswers func (Board board) =
    List.foldl (func >> (::)) [] board.answers


mapRemaining : (() -> a) -> Board -> List a
mapRemaining func board =
    List.repeat (remaining board) (func ())


mapFinal : (FinalSelection -> a) -> Board -> List a
mapFinal func ((Board board) as original) =
    if remaining original < 0 then
        [ func board.final ]

    else
        [ func FinalFuture ]


remaining : Board -> Int
remaining (Board board) =
    board.maxSize - List.length board.answers - 1
