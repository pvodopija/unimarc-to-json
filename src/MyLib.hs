module MyLib (convert) where

import Text.Parsec
import Data.Maybe
import Data.List

hline = replicate 310 '#'

data Indicator = Indicator Char | NOIND deriving Show
data Subfield = SecondaryField { getKey :: Char, getField :: Field } | Subfield { getKey :: Char, getValue :: String } 
data Field = Field {
                    getName :: String,
                    getInd1 :: Indicator,
                    getInd2 :: Indicator,
                    getSubfields :: [ Subfield ]
                    }
data Record = Record { getFields :: [ Field ] }
data Document =  Document { getRecords :: [ Record ] }

-- fml
tabs = \n -> replicate n '\t'
-- "Subfield '" ++ [ label ] ++ "' " ++ ( read . show $ value )
instance Show Subfield where
    show ( Subfield key value ) = "{\n" ++ tabs 8 ++ "\"key\" : " ++ [ '\"', key, '\"' ] ++ ",\n"
                                        ++ tabs 8 ++ "\"value\" : \"" ++ ( filter (/='\"') ( ( read . show ) value ) ) ++ "\"\n" ++ tabs 7 ++ "}"
    show ( SecondaryField key field ) = "{\n" ++ tabs 8 ++ "\"key\" : " ++ [ '\"', key, '\"' ] ++ ",\n"
                                        ++ tabs 8 ++ "\"field\" : " ++ ( show field ) ++ "\n" ++ tabs 7 ++ "}"

instance Show Document where
    show ( Document records ) = "{\n\t\"document\" : {\n"   ++ tabs 2 ++ "\"name\" : \"DOCUMENT\",\n"
                                                            ++ tabs 2 ++ "\"records\" :\n"
                                                            ++ tabs 3 ++ ( show records ) ++ "\n" ++ tabs 1 ++ "}"
                                                            ++ "\n}"

instance Show Record where
    show ( Record fields ) = "{\n" ++ tabs 4 ++  "\"name\" : \"record\",\n" ++ tabs 4 ++
                                                "\"fields\" :\n" ++ tabs 5 ++ ( show fields ) ++ "\n" ++ ( tabs 4 ) ++ "}"

instance Show Field where
    show ( Field label ind1 ind2 subfields ) = "{\n" ++ tabs 6 ++ "\"label\" : " ++ show label ++ ",\n" ++
                                                    tabs 6 ++ "\"firstIndicator\" : \"" ++ show ind1 ++ "\",\n" ++
                                                    tabs 6 ++ "\"secondIndicator\" : \"" ++ show ind2 ++ "\",\n" ++
                                                    tabs 6 ++ "\"subfields\" :\n" ++ tabs 7 ++ show subfields ++ "\n" ++ tabs 5 ++ "}"

                                                    -- sconcat [ show label, show ind1, show ind2, show subfields ] ++ "\n"

instance Eq Field where
    Field label1 _ _ _ == Field label2 _ _ _ = label1 == label2


-- Just some functions.
split' :: String -> [ String ]
tail' [] = []
tail' xs = tail xs
split' "" = [ [] ]
split' str = [ leftStr ] ++ split' ( tail' rightStr )
    where
        leftStr = takeWhile ( /= ' ' ) str
        rightStr = dropWhile ( /= ' ' ) str
split str = [ s | s <- split' str, s /= [] ]

-- Like concat but with space.
sconcat :: [ String ] -> String
sconcat [] = []
sconcat ( x:xs )
            | xs == [] = x
            | otherwise = x ++ " " ++ sconcat xs

toIndicator :: Char -> Indicator
toIndicator c
            | c == '#' = NOIND
            | otherwise = Indicator c

isSecondaryField :: Subfield -> Bool
isSecondaryField ( SecondaryField _ _ ) = True
isSecondaryField _ = False


dropAppendedSpaces :: String -> String
dropAppendedSpaces str
                    | last str == ' ' = dropAppendedSpaces ( init str )
                    | otherwise = str

genSecondarySubfields :: String -> [ Subfield ]
genSecondarySubfields' :: [ String ] -> [ Subfield ]

genSecondarySubfields' [] = []
genSecondarySubfields' ( x:xs )
            | xs == [] = []
            | otherwise = [ Subfield ( head x ) ( sconcat ( takeWhile ( \xs -> length xs > 1 ) xs ) ) ] ++ genSecondarySubfields' ( dropWhile ( \xs -> length xs > 1 ) xs )
genSecondarySubfields str = genSecondarySubfields' ( split ( dropAppendedSpaces str ) )
   
-- Parsing rules.
secondaryRule :: Parsec String () Field
secondaryRule = do  spaces
                    string "{{"
                    label       <- count 3 digit
                    spaces
                    indicator1  <- choice [ digit, ( char '#' ) ]
                    indicator2  <- choice [ digit, ( char '#' ) ]
                    many1 space
                    result1     <- many1 ( noneOf "}" )
                    spaces
                    string "}}"
                    spaces
                    return ( Field label ( toIndicator indicator1 ) ( toIndicator indicator2 ) ( genSecondarySubfields result1 ) )

-- secondaryRule = return ( Field "" ( toIndicator 'c' ) ( toIndicator 'x' ) [] )

subfieldRule :: Parsec String () Subfield
subfieldRule = do   char '['
                    subLabel    <- anyChar 
                    char ']'
                    value       <- manyTill anyChar ( try ( count 2 space ) )    -- choice [ count 2 space, many ( noneOf "{\n" ) ]
                    if value == []  
                    then do
                        field <- secondaryRule
                        return ( SecondaryField subLabel field )
                    else do
                        many ( char ' ' )
                        return ( Subfield subLabel ( dropAppendedSpaces value ) )

fieldRule :: Parsec String () Field
fieldRule = do  label       <- count 3 digit
                spaces
                indicator1  <- choice [ digit, ( char '#' ) ]
                indicator2  <- choice [ digit, ( char '#' ) ]
                spaces
                subfields   <- many1 subfieldRule
                many ( char ' ' )
                return ( Field label ( toIndicator indicator1 ) ( toIndicator indicator2 ) subfields )

recordRule :: Parsec String () Record
recordRule = do fields <- many fieldRule
                return ( Record fields )

documentRule :: Parsec String () Document
documentRule = do   records <- sepBy recordRule newline
                    return ( Document records )


-- Removing.
rmField :: [ Record ] -> String -> [ Record ]
rmField' :: [ Field ] -> String -> [ Field ]
rmField' [] _ = []
rmField' ( f:fields ) name  | getName f == name = rmField' fields name
                            | otherwise         = f : ( rmField' fields name )
rmField [] _  = []
rmField ( r:records ) name = Record ( rmField' ( getFields r ) name ) : rmField records name

rmSubOrSecondary :: Subfield -> String -> Char -> Maybe Subfield
rmSubOrSecondary sub name subName
                | subName == getKey sub =   Nothing
                | isSecondaryField sub  =   let x = rmSubfieldFromField ( getField sub ) name subName in
                                            if isNothing x
                                            then
                                                Nothing
                                            else
                                                return $ SecondaryField ( getKey sub ) $ fromJust x
                | otherwise             =   return sub
                        
rmSubfieldFromField :: Field -> String -> Char -> Maybe Field
rmSubfieldFromField f name subName
                | length subs == 0  = Nothing
                | otherwise         = return $ Field ( getName f ) ind1 ind2 [ sub | sub <- subs ]
                where
                    ind1 = getInd1 f
                    ind2 = getInd2 f
                    subs = catMaybes [ rmSubOrSecondary s name subName | s <- getSubfields f ]      -- Extracts from Just values, discards Nothing.


rmSubfield' :: [ Field ] -> String -> Char -> [ Field ]
rmSubfield' [] _ _ = []
rmSubfield' ( f:fields ) name subName    
                                        -- | getName f == name =   let r = rmSubfieldFromField f name subName in
                                                                -- if length ( getSubfields r ) == 0
                                                                -- then
                                                                --     rmSubfield' fields name subName
                                                                -- else
                                                                --     r : ( rmSubfield' fields name subName )
                | isNothing maybeField  = rmSubfield' fields name subName                               -- Delete since no Subfields are left.
                | otherwise             =   fromJust maybeField : ( rmSubfield' fields name subName )   -- Append modified Field. 
                where
                    maybeField = rmSubfieldFromField f name subName

rmSubfield :: [ Record ] -> String -> Char -> [ Record ]
rmSubfield [] _ _ = []
rmSubfield ( r:records ) fieldName subName = Record ( rmSubfield' ( getFields r ) fieldName subName ) : rmSubfield records fieldName subName

-- Adding.
addSubfieldtoField :: Field -> String -> Char -> String -> Field
addSubfieldtoField f fieldName newSubKey newSubValue
                | getName f == fieldName    = Field fieldName ( getInd1 f ) ( getInd2 f ) $ ( Subfield newSubKey newSubValue ) : ( getSubfields f )
                | otherwise                 = Field ( getName f ) ( getInd1 f ) ( getInd2 f ) $ editSecondary
                where
                    editSecondary = [   if isSecondaryField s
                                        then
                                            SecondaryField ( getKey s ) $ addSubfieldtoField ( getField s ) fieldName newSubKey newSubValue
                                        else 
                                            s
                                        | s <- getSubfields f   ]

addSubfield' :: [ Field ] -> String -> Char -> String -> [ Field ]
addSubfield' [] _ _ _ = []
addSubfield' ( f:fields ) fieldName newSubKey newSubValue = editedField : addSubfield' fields fieldName newSubKey newSubValue
                                                            where
                                                                editedField = addSubfieldtoField f fieldName newSubKey newSubValue

addSubfield :: [ Record ] -> String -> Char -> String -> [ Record ]
addSubfield [] _ _ _ = []
addSubfield ( r:records ) fieldName newSubKey newSubValue = Record ( addSubfield' ( getFields r ) fieldName newSubKey newSubValue  ) : addSubfield records fieldName newSubKey newSubValue

-- Editing.
editSubfieldInField :: Field -> String -> Char -> String -> Field
editSubfieldInField f fieldName subKey newSubValue
                | getName f == fieldName    = Field fieldName ( getInd1 f ) ( getInd2 f ) $ editedSubfields
                | otherwise                 = Field ( getName f ) ( getInd1 f ) ( getInd2 f ) $ editSecondary
                where
                    editedSubfields = [ if getKey s == subKey
                                        then
                                            Subfield subKey newSubValue
                                        else
                                            s 
                                        | s <- getSubfields f, not ( isSecondaryField s ) ]
                    editSecondary   = [ if isSecondaryField s
                                        then
                                            SecondaryField ( getKey s ) $ editSubfieldInField ( getField s ) fieldName subKey newSubValue
                                        else 
                                            s
                                        | s <- getSubfields f ]

editSubfield' :: [ Field ] -> String -> Char -> String -> [ Field ]
editSubfield' [] _ _ _ = []
editSubfield' ( f:fields ) fieldName subKey newSubValue =   editedField : editSubfield' fields fieldName subKey newSubValue
                                                            where
                                                                editedField = editSubfieldInField f fieldName subKey newSubValue

editSubfield :: [ Record ] -> String -> Char -> String -> [ Record ]
editSubfield [] _ _ _ = []
editSubfield ( r:records ) fieldName subKey newSubValue = Record ( editSubfield' ( getFields r ) fieldName subKey newSubValue  ) : editSubfield records fieldName subKey newSubValue

-- Searching
searchTextInField :: Field -> String -> Char -> String -> Maybe Field
searchTextInField f fieldName subKey searchValue
        | length matches > 0 = return f
        | otherwise          = Nothing     
        where
            matches = [ s | s <- getSubfields f, getKey s == subKey, isInfixOf searchValue ( getValue s ) ] 

searchSubfieldText' :: [ Field ] -> String -> Char -> String -> [ Field ]
searchSubfieldText' [] _ _ _ = []
searchSubfieldText' ( f:fields ) fieldName subKey searchValue
                | isNothing found = searchSubfieldText' fields fieldName subKey searchValue
                | otherwise       = fromJust found : searchSubfieldText' fields fieldName subKey searchValue
                where
                    found = searchTextInField f fieldName subKey searchValue

searchSubfieldText :: [ Record ] -> String -> Char -> String -> [ Record ]
searchSubfieldText [] _ _ _ = []
searchSubfieldText ( r:records ) fieldName subKey searchValue = Record ( searchSubfieldText' ( getFields r ) fieldName subKey searchValue  ) : searchSubfieldText records fieldName subKey searchValue

-- Group by Author.
dirtyCharacters = "#{}[]!.,'"       -- Basically, all the symbols should be here but...
cleanAuthor :: String -> String
cleanAuthor [] = []
cleanAuthor author = [ letter | letter <- author, not ( elem letter dirtyCharacters ) ]
-- cleanAuthor author  | elem ( head author ) dirtyCharacters  = cleanAuthor $ tail author
--                     | elem ( last author ) dirtyCharacters  = cleanAuthor $ init author
--                     | otherwise                             = author

isSameAuthor :: String -> String -> Bool
isSameAuthor author1 author2 = isInfixOf firstNameA1 cleanA2  && isInfixOf lastNameA1 cleanA2
        where
            firstNameA1 = cleanAuthor $ head $ split author1
            lastNameA1  = cleanAuthor $ last $ split author1
            cleanA2     = cleanAuthor author2

noAuthor = "anonymous author"
getAuthorFromFields :: [ Field ] -> String
getAuthorFromFields [] = noAuthor
getAuthorFromFields ( f:fields ) 
            | getName f == "200" = fromMaybe noAuthor maybeAuthor
            | otherwise = getAuthorFromFields fields
            where
                maybeAuthor = listToMaybe [ getValue s | s <- getSubfields f, getKey s == 'f' ]

getAuthorFromRecord :: Record -> String
getAuthorFromRecord record = getAuthorFromFields $ getFields record

rmDuplicateAuthors :: [ String ] -> [ String ]
rmDuplicateAuthors [] = []
rmDuplicateAuthors ( a:authors ) = a : rmDuplicateAuthors [ cleanAuthor author | author <- authors, not ( isSameAuthor a author ) ]

getAllAuthors :: [ Record ] -> [ String ]
getAllAuthors [] = []
getAllAuthors ( r:records ) = rmDuplicateAuthors $ getAuthorFromRecord r : getAllAuthors records

noTitle = "no title"
getTitleFromFields :: [ Field ] -> String
getTitleFromFields ( f:fields ) | getName f == "200" = fromMaybe noTitle maybeTitle
                                | otherwise = getTitleFromFields fields
                                where
                                    maybeTitle = listToMaybe [ getValue s | s <- getSubfields f, getKey s == 'a' ]

getTitleFromRecord :: Record -> String
getTitleFromRecord record = getTitleFromFields $ getFields record

getTitlesByAuthor :: [ Record ] -> String -> [ String ]
getTitlesByAuthor records author = [ getTitleFromRecord r | r <- records, isSameAuthor author ( getAuthorFromRecord r ) ]

getAuthorStats' :: [ Record ] -> [ String ]
getAuthorStats' records =   [   author ++ " (" ++ numberOfMatched  ++ ") " ++
                                intercalate ", " matchedTitles
                                | author <- getAllAuthors records,
                                let matchedTitles   = getTitlesByAuthor records author,
                                let numberOfMatched = show $ length matchedTitles   ]

getAuthorStats :: [ Record ] -> String
getAuthorStats records = "############ STATS ##########\n" ++ ( intercalate "\n" $ getAuthorStats' records )


convert :: String -> Either ParseError String
convert input = do 
    case ( runParser documentRule () "" input ) of
        Left err    -> Left err
        Right json  -> Right $ show $ Document $ getRecords json
        -- Right json  -> putStrLn . show $ getRecords json
        -- Right json  -> writeFile "unimarc.json" $ show $ Document $ addSubfield ( getRecords json ) "600" 'z' "INSERTED!"
        -- Right json  -> writeFile "unimarc.json" $ show $ Document $ rmSubfield ( getRecords json ) "200" 'a'
        -- Right json  -> writeFile "unimarc.json" $ show $ Document $ editSubfield ( getRecords json ) "200" 'a' "EDITED!"
        -- Right json  -> writeFile "unimarc.json" $ show $ Document $ searchSubfieldText ( getRecords json ) "225" 'a' "ris"
        -- Right json  -> putStrLn . read . show $ getAuthorStats $ getRecords json -- :: String







 

