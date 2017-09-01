module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Data
import Types exposing (Conflict, Source)


type alias Model =
    { allConflicts : List Conflict
    , filteredConflicts : List Conflict
    , selectedConflict : Maybe Conflict
    , searchParams : SearchParams
    }


type alias SearchParams =
    { searchString : String
    , familyMember : FamilyMember
    }


type FamilyMember
    = All
    | Sr
    | Jr
    | Ivanka
    | Jared
    | Melania
    | Eric


initModel : Model
initModel =
    { allConflicts = Data.conflictList
    , filteredConflicts = Data.conflictList
    , selectedConflict = Nothing
    , searchParams =
        { familyMember = All
        , searchString = ""
        }
    }



-- update


type Msg
    = Search String
    | SelectConflict Conflict
    | ChooseFamilyMember FamilyMember
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Search search ->
            let
                searchParams =
                    updateSearchString model.searchParams search

                filteredConflicts =
                    filterConflicts model.allConflicts searchParams
            in
                { model | searchParams = searchParams, filteredConflicts = filteredConflicts }

        ChooseFamilyMember familyMember ->
            let
                searchParams =
                    updateSearchFamilyMember model.searchParams familyMember

                filteredConflicts =
                    filterConflicts model.allConflicts searchParams
            in
                { model | searchParams = searchParams, filteredConflicts = filteredConflicts }

        SelectConflict conflict ->
            { model | selectedConflict = Just conflict }

        Clear ->
            initModel


updateSearchString : SearchParams -> String -> SearchParams
updateSearchString params searchString =
    { params | searchString = searchString }


updateSearchFamilyMember : SearchParams -> FamilyMember -> SearchParams
updateSearchFamilyMember params familyMember =
    { params | familyMember = familyMember }


filterConflicts : List Conflict -> SearchParams -> List Conflict
filterConflicts allConflicts searchParams =
    allConflicts
        |> filterBySearch searchParams.searchString
        |> filterByFamilyMember searchParams.familyMember


filterBySearch : String -> List Conflict -> List Conflict
filterBySearch searchString conflicts =
    let
        normalizedSearch =
            String.toUpper searchString

        containsWord record =
            String.contains normalizedSearch (String.toUpper (record.conflictingEntity ++ record.description))
    in
        List.filter containsWord conflicts


filterByFamilyMember : FamilyMember -> List Conflict -> List Conflict
filterByFamilyMember familyMember conflicts =
    case familyMember of
        All ->
            conflicts

        selected ->
            let
                matchesFamilyMember record =
                    stringToFamilyMember record.familyMember == selected
            in
                List.filter matchesFamilyMember conflicts



-- view


conflictPaneStyle : Attribute msg
conflictPaneStyle =
    style
        [ ( "float", "left" )
        ]


sourcePaneStyle : Attribute msg
sourcePaneStyle =
    style
        [ ( "float", "left" )
        , ( "width", "380px" )
        ]


view : Model -> Html Msg
view model =
    table
        [ style [ ( "width", "100%" ) ] ]
        [ tr []
            [ td [ style [ ( "font-size", "xx-large" ) ] ] [ text "Tracking Trump's Conflicts of Interest" ]
            ]
        , tr []
            [ td []
                [ input [ type_ "text", placeholder "Search", value model.searchParams.searchString, onInput Search ] []
                , td [] [ text (toString (List.length model.filteredConflicts) ++ " conflicts") ]
                , td [] [ button [ onClick Clear ] [ text "Clear" ] ]
                , td [] [ familyMemberRadios model ]
                ]
            ]
        , tr []
            [ td [ conflictPaneStyle ] (drawConflictRows model.filteredConflicts model.selectedConflict)
            , td [ sourcePaneStyle ] (drawSources model.selectedConflict)
            ]
        ]


drawConflictRows : List Conflict -> Maybe Conflict -> List (Html Msg)
drawConflictRows conflicts selectedConflict =
    let
        drawConflictRow : Conflict -> Html Msg
        drawConflictRow conflict =
            tr [ onClick (SelectConflict conflict) ]
                [ td [ style [ ( "width", "70px" ) ], classList [ ( "selected", isSelected selectedConflict conflict ) ] ] [ text conflict.familyMember ]
                , td [ style [ ( "width", "70px" ) ], classList [ ( "selected", isSelected selectedConflict conflict ) ] ] [ text conflict.category ]
                , td [ style [ ( "width", "100px" ) ], classList [ ( "selected", isSelected selectedConflict conflict ) ] ] [ text conflict.conflictingEntity ]
                , td [ style [ ( "width", "400px" ) ], classList [ ( "selected", isSelected selectedConflict conflict ) ] ] [ text conflict.description ]
                ]
    in
        List.map drawConflictRow conflicts


isSelected : Maybe Conflict -> Conflict -> Bool
isSelected selectedConflict conflict =
    case selectedConflict of
        Nothing ->
            False

        Just selected ->
            selected == conflict


drawSources : Maybe Conflict -> List (Html Msg)
drawSources conflict =
    let
        drawSourceRow : Source -> Html Msg
        drawSourceRow source =
            tr []
                [ td [ style [ ( "width", "150px" ) ] ] [ a [ href source.link ] [ text source.name ] ]
                , td [ style [ ( "width", "80px" ) ] ] [ text source.date ]
                ]
    in
        case conflict of
            Nothing ->
                [ h3 [] [ text "" ] ]

            Just conflict ->
                conflict.sources
                    |> List.map drawSourceRow


familyMemberRadios : Model -> Html Msg
familyMemberRadios model =
    let
        radio familyMember =
            label
                [ style [ ( "padding", "20px" ) ]
                ]
                [ input
                    [ type_ "radio"
                    , name "familyMember"
                    , onClick (ChooseFamilyMember familyMember)
                    , checked (model.searchParams.familyMember == familyMember)
                    ]
                    []
                , text (familyMemberToString familyMember)
                ]
    in
        fieldset []
            [ radio All
            , radio Sr
            , radio Jr
            , radio Ivanka
            , radio Jared
            , radio Melania
            , radio Eric
            ]


familyMemberToString : FamilyMember -> String
familyMemberToString person =
    case person of
        All ->
            "All"

        Sr ->
            "Trump"

        Jr ->
            "Junior"

        Ivanka ->
            "Ivanka"

        Jared ->
            "Jared"

        Melania ->
            "Melania"

        Eric ->
            "Eric"


stringToFamilyMember : String -> FamilyMember
stringToFamilyMember person =
    case person of
        "All" ->
            All

        "Trump" ->
            Sr

        "Junior" ->
            Jr

        "Ivanka" ->
            Ivanka

        "Jared" ->
            Jared

        "Melania" ->
            Melania

        "Eric" ->
            Eric

        _ ->
            Sr


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
