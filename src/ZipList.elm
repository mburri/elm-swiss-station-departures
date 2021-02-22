module ZipList exposing
    ( ZipList
    , back
    , current
    , empty
    , forward
    , new
    , toList
    )


type ZipList a
    = ZipList (List a) a (List a)
    | Empty


empty : ZipList a
empty =
    Empty


new : a -> List a -> ZipList a
new element list =
    ZipList [] element list


back : ZipList a -> ZipList a
back zipList =
    case zipList of
        Empty ->
            zipList

        ZipList before element after ->
            case before of
                [] ->
                    zipList

                x :: xs ->
                    ZipList xs x (element :: after)


forward : ZipList a -> ZipList a
forward zipList =
    case zipList of
        Empty ->
            zipList

        ZipList before element after ->
            case after of
                [] ->
                    zipList

                x :: xs ->
                    ZipList (element :: before) x xs


current : ZipList a -> Maybe a
current zipList =
    case zipList of
        Empty ->
            Nothing

        ZipList _ element _ ->
            Just element


toList : ZipList a -> List a
toList zipList =
    case zipList of
        Empty ->
            []

        ZipList before element after ->
            List.concat
                [ List.reverse before
                , [ element ]
                , after
                ]
