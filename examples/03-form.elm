import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import String
import Regex


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  , message : String
  , colour : String
  }


model : Model
model =
  Model "" "" "" 0 "" "green"



-- UPDATE


type Msg
    = SubmitForm
    | Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = Result.withDefault 0 (String.toInt age) }

    SubmitForm ->
      viewValidation model


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "number", placeholder "Age", onInput Age] []
    , button [ onClick SubmitForm ] [text "Submit"]
    , div [ style [("color", model.colour)] ] [ text model.message ]
    ]


viewValidation : Model -> Model
viewValidation model =
      if model.password /= model.passwordAgain then
        { model | colour = "red", message = "Passwords do not match!" }
      else if String.length model.password < 8 then
        { model | colour = "red", message = "Password is too short!" }
      else if not (Regex.contains (Regex.regex "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*(_|[^\\w])).+$") model.password) then
        { model | colour = "red", message = "Password should contain upper and lower case characters, digits and symbols" }
      else
        { model | colour = "green", message = "OK" }
