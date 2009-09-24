{- |
   Module      :  Text.ParserCombinators.Parsec.Rfc2822
   Copyright   :  (c) 2008 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides parsers for the grammar defined in
   RFC2822, \"Internet Message Format\",
   <http://www.faqs.org/rfcs/rfc2822.html>.

   /Please note:/ The module is not particularly well tested.


   Addendum for Nonstandard Version:
   This module deviates from the RFC currently in 
        * Allowing for non-standard line endings.

   These allowances are subject to change, and should not be
   used when parsing incoming messages, only for parsing messages
   that have been stored on disk. The goal of these nonstandard
   Parsers is to provide a higher probability of parsing Common_ 
   Headers (rather than only those explicitly defined in the RFC)
   as well as allowing for potential oddities / changes that may
   occur during storage of an email message. These parsers have
   be rebranded so as not to conflict with the standard parsers
   available from the excellent 'hsemail' package, upon which
   this package depends. For patches to this package only (namely
   'hsemail-ns', patches should be sent to <jfredett@gmail.com>, 
   for patches to the Proper parsers, you can send them to the
   original maintainer. 
-}

module Text.ParserCombinators.Parsec.Rfc2822NS where

import System.Time
import Data.Char ( ord )
import Data.List ( intersperse )
import Control.Monad ( liftM )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Rfc2234NS hiding ( quotedPair, quotedString )

-- * Useful parser combinators

-- |Return @Nothing@ if the given parser doesn't match. This
-- combinator is included in the latest parsec distribution as
-- @optionMaybe@, but ghc-6.6.1 apparently doesn't have it.

maybeOption    :: GenParser tok st a -> GenParser tok st (Maybe a)
maybeOption     = option Nothing . liftM Just

-- |@unfold@ @=@ @between (optional cfws) (optional cfws)@

unfold          :: CharParser a b -> CharParser a b
unfold           = between (optional cfws) (optional cfws)

-- |Construct a parser for a message header line from the
-- header's name and a parser for the body.

header          :: String -> CharParser a b -> CharParser a b
header n p       = let nameString = caseString (n ++ ":")
                   in
                   between nameString crlf p <?> (n ++ " header line")

-- |Like 'header', but allows the obsolete white-space rules.

obsHeader       :: String -> CharParser a b -> CharParser a b
obsHeader  n p   = let nameString = caseString n >> many wsp >> char ':'
                   in
                   between nameString crlf p <?> ("obsolete " ++ n ++ " header line")


-- ** Primitive Tokens (section 3.2.1)

-- |Match any US-ASCII non-whitespace control character.

noWsCtl         :: CharParser a Char
noWsCtl         = satisfy (\c -> ord c `elem` ([1..8] ++ [11,12] ++ [14..31] ++ [127]))
                  <?> "US-ASCII non-whitespace control character"

-- |Match any US-ASCII character except for @\r@, @\n@.

text            :: CharParser a Char
text            = satisfy (\c -> ord c `elem` ([1..9] ++ [11,12] ++ [14..127]))
                  <?> "US-ASCII character (excluding CR and LF)"

-- |Match any of the RFC's \"special\" characters: @()\<\>[]:;\@,.\\\"@.

specials        :: CharParser a Char
specials        = oneOf "()<>[]:;@,.\\\""   <?> "one of ()<>[]:;@,.\\\""


-- ** Quoted characters (section 3.2.2)

-- |Match a \"quoted pair\". All characters matched by 'text' may be
-- quoted. Note that the parsers returns /both/ characters, the
-- backslash and the actual content.

quotedPair      :: CharParser a String
quotedPair      = do { char '\\'; r <- text; return ['\\',r] }
                  <?> "quoted pair"


-- ** Folding white space and comments (section 3.2.3)

-- |Match \"folding whitespace\". That is any combination of 'wsp' and
-- 'crlf' followed by 'wsp'.

fws             :: CharParser a String
fws             = do r <- many1 $ choice [ blanks, linebreak]
                     return (concat r)
    where
    blanks      = many1 wsp
    linebreak   = try $ do { r1 <- crlf; r2 <- blanks; return (r1 ++ r2) }

-- |Match any non-whitespace, non-control character except for \"@(@\",
-- \"@)@\", and \"@\\@\". This is used to describe the legal content of
-- 'comment's.
--
-- /Note/: This parser accepts 8-bit characters, even though this is
-- not legal according to the RFC. Unfortunately, 8-bit content in
-- comments has become fairly common in the real world, so we'll just
-- accept the fact.

ctext           :: CharParser a Char
ctext           = noWsCtl   <|> satisfy (\c -> ord c `elem` ([33..39] ++ [42..91] ++ [93..126] ++ [128..255]))
                  <?> "any regular character (excluding '(', ')', and '\\')"

-- |Match a \"comments\". That is any combination of 'ctext',
-- 'quotedPair 's, and 'fws' between brackets. Comments may nest.

comment         :: CharParser a String
comment         = do char '('
                     r1 <- many ccontent
                     r2 <- option [] fws
                     char ')'
                     return ("(" ++ concat r1 ++ r2 ++ ")")
                  <?> "comment"
    where
    ccontent    = try $ do r1 <- option [] fws
                           r2 <- choice [many1 ctext, quotedPair , comment]
                           return (r1 ++ r2)

-- |Match any combination of 'fws' and 'comments'.

cfws            :: CharParser a String
cfws            = do r <- many1 $ choice [ fws, comment ]
                     return (concat r)

-- ** Atom (section 3.2.4)

-- |Match any US-ASCII character except for control characters,
-- 'specials', or space. 'atom' and 'dotAtom' are made up of this. 

atext           :: CharParser a Char
atext           = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"
                  <?> "US-ASCII character (excluding controls, space, and specials)"

-- |Match one or more 'atext' characters and skip any preceeding or
-- trailing 'cfws'.

atom            :: CharParser a String
atom            = unfold (many1 atext <?> "atom")

-- |Match 'dotAtomText' and skip any preceeding or trailing 'cfws'. 

dotAtom        :: CharParser a String 
dotAtom        = unfold (dotAtomText <?> "dot atom") 

-- |Match two or more 'atext's interspersed by dots.

dotAtomText   :: CharParser a String 
dotAtomText   = do r <- sepBy1 (many1 atext) (char '.') 
                   return (concat (intersperse "." r))
                 <?> "dot atom content"


-- ** Quoted strings (section 3.2.5)

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

qtext           :: CharParser a Char
qtext            = noWsCtl   <|> satisfy (\c -> ord c `elem` ([33] ++ [35..91] ++ [93..126]))
                <?> "US-ASCII character (excluding '\\', and '\"')"

-- |Match either 'qtext' or 'quotedPair '.

qcontent        :: CharParser a String
qcontent        = many1 qtext <|> quotedPair 
               <?> "quoted string content"

-- |Match any number of 'qcontent' between double quotes. Any 'cfws'
-- preceeding or following the \"atom\" is skipped automatically.

quotedString   :: CharParser a String
quotedString    = unfold (do dquote
                             r1 <- many (do r1 <- option [] fws
                                            r2 <- qcontent
                                            return (r1 ++ r2))
                             r2 <- option [] fws
                             dquote
                             return ("\"" ++ concat r1 ++ r2 ++ "\""))
                  <?> "quoted string"


-- * Miscellaneous tokens (section 3.2.6)

-- |Match either 'atom' or 'quotedString'.

word            :: CharParser a String
word            = atom <|> quotedString     <?> "word"

-- |Match either one or more 'word's or an 'obsPhrase'.

phrase          :: CharParser a [String]
phrase          = {- many1 word <?> "phrase" <|> -} obsPhrase

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

utext           :: CharParser a Char
utext           = noWsCtl   <|> satisfy (\c -> ord c `elem` [33..126])
                  <?> "regular US-ASCII character (excluding '\\', and '\"')"

-- |Match any number of 'utext' tokens.
--
-- \"Unstructured text\" is used in free text fields such as 'subject'.
-- Please note that any comments or whitespace that prefaces or
-- follows the actual 'utext' is /included/ in the returned string.

unstructured    :: CharParser a String
unstructured    = do r1 <- option [] fws
                     r2 <- many (do r3 <- utext
                                    r4 <- option [] fws
                                    return (r3 : r4))
                     return (r1 ++ concat r2)
                  <?> "unstructured text"


-- * Date and Time Specification (section 3.3)

-- |Parse a date and time specification of the form
--
-- >   Thu, 19 Dec 2002 20:35:46 +0200
--
-- where the weekday specification \"@Thu,@\" is optional. The parser
-- returns a 'CalendarTime', which is set to the appropriate values.
-- Note, though, that not all fields of 'CalendarTime' will
-- necessarily be set correctly! Obviously, when no weekday has been
-- provided, the parser will set this field to 'Monday' - regardless
-- of whether the day actually is a monday or not. Similarly, the day
-- of the year will always be returned as @0@. The timezone name will
-- always be empty: @\"\"@.
--
-- Nor will the 'dateTime' parser perform /any/ consistency checking.
-- It will accept
--
-- >    40 Apr 2002 13:12 +0100
--
-- as a perfectly valid date.
--
-- In order to get all fields set to meaningful values, and in order
-- to verify the date's consistency, you will have to feed it into any
-- of the conversion routines provided in "System.Time", such as
-- 'toClockTime'. (When doing this, keep in mind that most functions
-- return /local time/. This will not necessarily be the time you're
-- expecting.)

dateTime       :: CharParser a CalendarTime
dateTime       = do wd <- option Monday (try (do wd <- dayOfWeek 
                                                 char ','
                                                 return wd))
                    (y,m,d) <- date
                    fws
                    (td,z) <- time
                    optional cfws
                    return (CalendarTime y m d (tdHour td) (tdMin td) (tdSec td) 0 wd 0 "" z False)
                 <?> "date/time specification"

-- |This parser will match a 'dayName', optionally wrapped in folding
-- whitespace, or an 'obsDayOfWeek' and return it's 'Day' value.  

dayOfWeek     :: CharParser a Day 
dayOfWeek     = try (between (optional fws) (optional fws) dayName <?> "name of a day-of-the-week") 
               <|> obsDayOfWeek  

-- |This parser will the abbreviated weekday names (\"@Mon@\", \"@Tue@\", ...)
-- and return the appropriate 'Day' value.

dayName        :: CharParser a Day
dayName        =     do { caseString "Mon"; return Monday }
                  <|> do { try (caseString "Tue"); return Tuesday }
                  <|> do { caseString "Wed"; return Wednesday }
                  <|> do { caseString "Thu"; return Thursday }
                  <|> do { caseString "Fri"; return Friday }
                  <|> do { try (caseString "Sat"); return Saturday }
                  <|> do { caseString "Sun"; return Sunday }
                  <?> "name of a day-of-the-week"

-- |This parser will match a date of the form \"@dd:mm:yyyy@\" and return
-- a tripple of the form (Int,Month,Int) - corresponding to
-- (year,month,day).

date            :: CharParser a (Int,Month,Int)
date            = do d <- day
                     m <- month
                     y <- year
                     return (y,m,d)
                  <?> "date specification"

-- |This parser will match a four digit number and return it's integer
-- value. No range checking is performed.

year            :: CharParser a Int
year            = do y <- manyN 4 digit
                     return (read y :: Int)
                  <?> "year"

-- |This parser will match a 'monthName', optionally wrapped in
-- folding whitespace, or an 'obsMonth' and return it's 'Month' 
-- value.

month           :: CharParser a Month
month           =     try (between (optional fws) (optional fws) monthName <?> "month name")
                  <|> obsMonth 


-- |This parser will the abbreviated month names (\"@Jan@\", \"@Feb@\", ...)
-- and return the appropriate 'Month' value.

monthName      :: CharParser a Month
monthName      =     do { try (caseString "Jan"); return January }
                  <|> do { caseString "Feb"; return February }
                  <|> do { try (caseString "Mar"); return March }
                  <|> do { try (caseString "Apr"); return April }
                  <|> do { caseString "May"; return May }
                  <|> do { try (caseString "Jun"); return June }
                  <|> do { caseString "Jul"; return July }
                  <|> do { caseString "Aug"; return August }
                  <|> do { caseString "Sep"; return September }
                  <|> do { caseString "Oct"; return October }
                  <|> do { caseString "Nov"; return November }
                  <|> do { caseString "Dec"; return December }
                  <?> "month name"

-- |Match either an 'obsDay', or a one or two digit number and return it. 

day             :: CharParser a Int
day             =  try (do { optional fws; r <- manyNtoM 1 2 digit; return (read r :: Int) }) <|> obsDay 
                   <?> "day"

-- |This parser will match a 'timeOfDay' specification followed by a 
-- 'zone'. It returns the tuple (TimeDiff,Int) corresponding to the
-- return values of either parser.

time            :: CharParser a (TimeDiff,Int)
time            = do t <- timeOfDay 
                     fws
                     z <- zone
                     return (t,z)
                  <?> "time and zone specification"

-- |This parser will match a time-of-day specification of \"@hh:mm@\" or
-- \"@hh:mm:ss@\" and return the corrsponding time as a 'TimeDiff'.

timeOfDay     :: CharParser a TimeDiff 
timeOfDay     = do h <- hour 
                   char ':'
                   m <- minute
                   s <- option 0 (do { char ':'; second } )
                   return (TimeDiff 0 0 0 h m s 0)
             <?> "time specification"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking is performed.

hour            :: CharParser a Int
hour            = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "hour"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking is performed.

minute          :: CharParser a Int
minute          = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "minute"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking takes place.

second          :: CharParser a Int
second          = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "second"

-- |This parser will match a timezone specification of the form
-- \"@+hhmm@\" or \"@-hhmm@\" and return the zone's offset to UTC in
-- seconds as an integer. 'obsZone' is matched as well. 

zone            :: CharParser a Int
zone            = (    do char '+'
                          h <- hour
                          m <- minute
                          return (((h*60)+m)*60)
                   <|> do char '-'
                          h <- hour
                          m <- minute
                          return (-((h*60)+m)*60)
                   <?> "time zone"
                  )
                  <|> obsZone 


-- * Address Specification (section 3.4)

-- |A NameAddr is composed of an optional realname a mandatory
-- e-mail 'address'.

data NameAddr = NameAddr { nameAddrName :: Maybe String
                         , nameAddrAddr :: String 
                         }
                deriving (Show,Eq)

-- |Parse a single 'mailbox' or an address 'group' and return the
-- address(es).

address         :: CharParser a [NameAddr]
address         = try (do { r <- mailbox; return [r] }) <|> group
               <?> "address"

-- |Parse a 'nameAddr' or an 'addrSpec' and return the  
-- address.

mailbox         :: CharParser a NameAddr
mailbox         =  try nameAddr 
               <|> liftM (NameAddr Nothing) addrSpec
               <?> "mailbox"

-- |Parse an 'angleAddr', optionally prefaced with a 'displayName', 
-- and return the address.

nameAddr       :: CharParser a NameAddr 
nameAddr       = do name <- maybeOption displayName 
                    addr <- angleAddr 
                    return (NameAddr name addr)
               <?> "name address"

-- |Parse an 'angle_addr' or an 'obsAngleAddr' and return the address.  

angleAddr      :: CharParser a String 
angleAddr      = try (unfold (do char '<' 
                                 r <- addrSpec 
                                 char '>'
                                 return r)
                      <?> "angle address"
                     )
                 <|> obsAngleAddr  

-- |Parse a \"group\" of addresses. That is a 'displayName', followed
-- by a colon, optionally followed by a 'mailboxList', followed by a 
-- semicolon. The found address(es) are returned - what may be none.
-- Here is an example:
--
-- >    parse group "" "my group: user1@example.org, user2@example.org;"
--
-- This input comes out as:
--
-- >    Right ["user1@example.org","user2@example.org"]

group           :: CharParser a [NameAddr]
group           = do displayName
                     char ':'
                     r <- option [] mailboxList 
                     unfold $ char ';'
                     return r
                  <?> "address group"

-- |Parse and return a 'phrase'.

displayName    :: CharParser a String
displayName    = phrase >>= return . unwords
                  <?> "display name"

-- |Parse a list of 'mailbox' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

mailboxList    :: CharParser a [NameAddr] 
mailboxList    = sepBy mailbox (char ',') <?> "mailbox list" 

-- |Parse a list of 'address' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

addressList    :: CharParser a [NameAddr] 
addressList    = do { r <-sepBy address (char ','); return (concat r) } 
                  <?> "address list"


-- ** Addr-spec specification (section 3.4.1)

-- |Parse an \"address specification\". That is a 'localPart', followed
-- by an \"@\@@\" character, followed by a 'domain'. Return the complete
-- address as 'String', ignoring any whitespace or any comments.

addrSpec       :: CharParser a String 
addrSpec       = do r1 <- localPart 
                    char '@'
                    r2 <- domain
                    return (r1 ++ "@" ++ r2)
                  <?> "address specification"

-- |Parse and return a \"local part\" of an 'addrSpec'. That is either 
-- a 'dotAtom' or a 'quotedString'. 

localPart      :: CharParser a String
localPart      = dotAtom <|> quotedString 
                  <?> "address' local part"

-- |Parse and return a \"domain part\" of an 'addrSpec'. That is either 
-- a 'dotAtom' or a 'domainLiteral'.  

domain          :: CharParser a String
domain          = dotAtom <|> domainLiteral  
                  <?> "address' domain part"

-- |Parse a \"domain literal\". That is a \"@[@\" character, followed by
-- any amount of 'dcontent', followed by a terminating \"@]@\"
-- character. The complete string is returned verbatim.

domainLiteral  :: CharParser a String 
domainLiteral  = unfold (do char '[' 
                            r <- many $ do { optional fws; dcontent }
                            optional fws
                            char ']'
                            return ("[" ++ concat r ++ "]"))
                 <?> "domain literal"

-- |Parse and return any characters that are legal in a
-- 'domainLiteral'. That is 'dtext' or a 'quotedPair '. 

dcontent        :: CharParser a String
dcontent        = many1 dtext <|> quotedPair 
                  <?> "domain literal content"

-- |Parse and return any ASCII characters except \"@[@\", \"@]@\", and
-- \"@\\@\".

dtext           :: CharParser a Char
dtext           = noWsCtl  
                  <|> satisfy (\c -> ord c `elem` ([33..90] ++ [94,127]))
                  <?> "character (excluding '[', ']', and '\\')"


-- * Overall message syntax (section 3.5)

-- |This data type repesents a parsed Internet Message as defined in
-- this RFC. It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data Message = Message [Field] String
      deriving (Show)

-- |Parse a complete message as defined by this RFC and it broken down
-- into the separate header fields and the message body. Header lines,
-- which contain syntax errors, will not cause the parser to abort.
-- Rather, these headers will appear as 'OptionalField's (which are
-- unparsed) in the resulting 'Message'. A message must be really,
-- really badly broken for this parser to fail.
--
-- This behaviour was chosen because it is impossible to predict what
-- the user of this module considers to be a fatal error;
-- traditionally, parsers are very forgiving when it comes to Internet
-- messages.
--
-- If you want to implement a really strict parser, you'll have to put
-- the appropriate parser together yourself. You'll find that this is
-- rather easy to do. Refer to the 'fields' parser for further details.

message         :: CharParser a Message
message         = do f <- fields
                     b <- option [] (do crlf
                                        body)
                     return (Message f b)

-- |This parser will return a message body as specified by this RFC;
-- that is basically any number of 'text' characters, which may be
-- divided into separate lines by 'crlf'.

body            :: CharParser a String
body            = do r1 <- many (try (do line <- many text
                                         eol <- crlf
                                         return (line ++ eol)))
                     r2 <- many text
                     return (concat r1 ++ r2)


-- * Field definitions (section 3.6)

-- |This data type represents any of the header fields defined in this
-- RFC. Each of the various instances contains with the return value
-- of the corresponding parser.

data Field      = OptionalField       String String
                | From                [NameAddr]
                | Sender              NameAddr
                | ReturnPath          String
                | ReplyTo             [NameAddr]
                | To                  [NameAddr]
                | Cc                  [NameAddr]
                | Bcc                 [NameAddr]
                | MessageID           String
                | InReplyTo           [String]
                | References          [String]
                | Subject             String
                | Comments            String
                | Keywords            [[String]]
                | Date                CalendarTime
                | ResentDate          CalendarTime
                | ResentFrom          [NameAddr]
                | ResentSender        NameAddr
                | ResentTo            [NameAddr]
                | ResentCc            [NameAddr]
                | ResentBcc           [NameAddr]
                | ResentMessageID     String
                | ResentReplyTo       [NameAddr]
                | Received            ([(String,String)], CalendarTime)
                | ObsReceived         [(String,String)]
                deriving (Show)

-- |This parser will parse an arbitrary number of header fields as
-- defined in this RFC. For each field, an appropriate 'Field' value
-- is created, all of them making up the 'Field' list that this parser
-- returns.
--
-- If you look at the implementation of this parser, you will find
-- that it uses Parsec's 'try' modifier around /all/ of the fields.
-- The idea behind this is that fields, which contain syntax errors,
-- fall back to the catch-all 'optionalField'. Thus, this parser will 
-- hardly ever return a syntax error -- what conforms with the idea
-- that any message that can possibly be accepted /should/ be.

fields          :: CharParser a [Field]
fields          = many (    try (do { r <- from; return (From r) })
                        <|> try (do { r <- sender; return (Sender r) })
                        <|> try (do { r <- returnPath; return (ReturnPath r) })
                        <|> try (do { r <- replyTo; return (ReplyTo r) })
                        <|> try (do { r <- to; return (To r) })
                        <|> try (do { r <- cc; return (Cc r) })
                        <|> try (do { r <- bcc; return (Bcc r) })
                        <|> try (do { r <- messageId; return (MessageID r) }) 
                        <|> try (do { r <- inReplyTo; return (InReplyTo r) }) 
                        <|> try (do { r <- references; return (References r) })
                        <|> try (do { r <- subject; return (Subject r) })
                        <|> try (do { r <- comments; return (Comments r) })
                        <|> try (do { r <- keywords; return (Keywords r) })
                        <|> try (do { r <- origDate; return (Date r) }) 
                        <|> try (do { r <- resentDate; return (ResentDate r) }) 
                        <|> try (do { r <- resentFrom; return (ResentFrom r) }) 
                        <|> try (do { r <- resentSender; return (ResentSender r) }) 
                        <|> try (do { r <- resentTo; return (ResentTo r) })
                        <|> try (do { r <- resentCc; return (ResentCc r) }) 
                        <|> try (do { r <- resentBcc; return (ResentBcc r) }) 
                        <|> try (do { r <- resentMsgId; return (ResentMessageID r) })  
                        <|> try (do { r <- received; return (Received r) })
                         -- catch all
                        <|> (do { (name,cont) <- optionalField; return (OptionalField name cont) }) 
                       )


-- ** The origination date field (section 3.6.1)

-- |Parse a \"@Date:@\" header line and return the date it contains a
-- 'CalendarTime'.

origDate       :: CharParser a CalendarTime 
origDate       = header "Date" dateTime 


-- ** Originator fields (section 3.6.2)

-- |Parse a \"@From:@\" header line and return the 'mailboxList' 
-- address(es) contained in it.

from            :: CharParser a [NameAddr]
from            = header "From" mailboxList 

-- |Parse a \"@Sender:@\" header line and return the 'mailbox' address
-- contained in it.

sender          :: CharParser a NameAddr
sender          = header "Sender" mailbox

-- |Parse a \"@Reply-To:@\" header line and return the 'addressList' 
-- address(es) contained in it.

replyTo        :: CharParser a [NameAddr]
replyTo        = header "Reply-To" addressList 


-- ** Destination address fields (section 3.6.3)

-- |Parse a \"@To:@\" header line and return the 'addressList' 
-- address(es) contained in it.

to              :: CharParser a [NameAddr]
to              = header "To" addressList 

-- |Parse a \"@Cc:@\" header line and return the 'addressList' 
-- address(es) contained in it.

cc              :: CharParser a [NameAddr]
cc              = header "Cc" addressList 

-- |Parse a \"@Bcc:@\" header line and return the 'addressList' 
-- address(es) contained in it.

bcc             :: CharParser a [NameAddr]
bcc             = header "Bcc" (try addressList <|> do { optional cfws; return [] }) 

-- ** Identification fields (section 3.6.4)

-- |Parse a \"@Message-Id:@\" header line and return the 'msgId' 
-- contained in it.

messageId      :: CharParser a String 
messageId      = header "Message-ID" msgId 

-- |Parse a \"@In-Reply-To:@\" header line and return the list of
-- 'msgId's contained in it. 

inReplyTo     :: CharParser a [String] 
inReplyTo     = header "In-Reply-To" (many1 msgId)  

-- |Parse a \"@References:@\" header line and return the list of
-- 'msgId's contained in it. 

references      :: CharParser a [String]
references      = header "References" (many1 msgId) 

-- |Parse a \"@message ID:@\" and return it. A message ID is almost
-- identical to an 'angleAddr', but with stricter rules about folding 
-- and whitespace.

msgId          :: CharParser a String 
msgId          = unfold (do char '<' 
                            idl <- idLeft 
                            char '@'
                            idr <- idRight 
                            char '>'
                            return ("<" ++ idl ++ "@" ++ idr ++ ">"))
               <?> "message ID"

-- |Parse a \"left ID\" part of a 'msgId'. This is almost identical to 
-- the 'localPart' of an e-mail address, but with stricter rules
-- about folding and whitespace.

idLeft         :: CharParser a String 
idLeft         = dotAtomText <|> noFoldQuote    
                  <?> "left part of an message ID"

-- |Parse a \"right ID\" part of a 'msgId'. This is almost identical to 
-- the 'domain' of an e-mail address, but with stricter rules about
-- folding and whitespace.

idRight        :: CharParser a String 
idRight        = dotAtomText <|> noFoldLiteral    
                  <?> "right part of an message ID"

-- |Parse one or more occurences of 'qtext' or 'quotedPair ' and
-- return the concatenated string. This makes up the 'idLeft' of a 
-- 'msgId'. 

noFoldQuote   :: CharParser a String  
noFoldQuote   = do dquote  
                   r <- many (many1 qtext <|> quotedPair )
                   dquote
                   return ("\"" ++ concat r ++ "\"")
                 <?> "non-folding quoted string"

-- |Parse one or more occurences of 'dtext' or 'quotedPair ' and
-- return the concatenated string. This makes up the 'idRight' of a 
-- 'msgId'. 

noFoldLiteral :: CharParser a String  
noFoldLiteral = do char '['  
                   r <- many (many1 dtext <|> quotedPair )
                   char ']'
                   return ("\"" ++ concat r ++ "\"")
                   return ("[" ++ concat r ++ "]")
                 <?> "non-folding domain literal"


-- ** Informational fields (section 3.6.5)

-- |Parse a \"@Subject:@\" header line and return it's contents verbatim.

subject         :: CharParser a String
subject         = header "Subject" unstructured

-- |Parse a \"@Comments:@\" header line and return it's contents verbatim.

comments        :: CharParser a String
comments        = header "Comments" unstructured

-- |Parse a \"@Keywords:@\" header line and return the list of 'phrase's
-- found. Please not that each phrase is again a list of 'atom's, as
-- returned by the 'phrase' parser.

keywords        :: CharParser a [[String]]
keywords        = header "Keywords" (do r1 <- phrase
                                        r2 <- many (do char ','
                                                       phrase)
                                        return (r1:r2))


-- ** Resent fields (section 3.6.6)

-- |Parse a \"@Resent-Date:@\" header line and return the date it
-- contains as 'CalendarTime'.

resentDate     :: CharParser a CalendarTime 
resentDate     = header "Resent-Date" dateTime 

-- |Parse a \"@Resent-From:@\" header line and return the 'mailboxList' 
-- address(es) contained in it.

resentFrom     :: CharParser a [NameAddr] 
resentFrom     = header "Resent-From" mailboxList  


-- |Parse a \"@Resent-Sender:@\" header line and return the 'mailboxList' 
-- address(es) contained in it.

resentSender   :: CharParser a NameAddr 
resentSender   = header "Resent-Sender" mailbox 


-- |Parse a \"@Resent-To:@\" header line and return the 'mailbox'
-- address contained in it.

resentTo       :: CharParser a [NameAddr]
resentTo       = header "Resent-To" addressList 

-- |Parse a \"@Resent-Cc:@\" header line and return the 'addressList' 
-- address(es) contained in it.

resentCc       :: CharParser a [NameAddr] 
resentCc       = header "Resent-Cc" addressList  

-- |Parse a \"@Resent-Bcc:@\" header line and return the 'addressList' 
-- address(es) contained in it. (This list may be empty.)

resentBcc      :: CharParser a [NameAddr] 
resentBcc      = header "Resent-Bcc" (    try addressList  
                                       <|> do optional cfws
                                              return []
                                      )
                  <?> "Resent-Bcc: header line"

-- |Parse a \"@Resent-Message-ID:@\" header line and return the 'msgId' 
-- contained in it.

resentMsgId   :: CharParser a String  
resentMsgId   = header "Resent-Message-ID" msgId  


-- ** Trace fields (section 3.6.7)

returnPath     :: CharParser a String
returnPath     = header "Return-Path:" path

path            :: CharParser a String
path            = unfold (    do char '<'
                                 r <- choice [ try addrSpec, do { cfws; return [] } ] 
                                 char '>'
                                 return ("<" ++ r ++ ">")
                          <|> obsPath
                         )
                  <?> "return path spec"

received        :: CharParser a ([(String,String)], CalendarTime)
received        = header "Received" (do r1 <- nameValList  
                                        char ';'
                                        r2 <- dateTime
                                        return (r1,r2))

nameValList   :: CharParser a [(String,String)]  
nameValList   = do optional cfws  
                   many1 nameValPair 
                  <?> "list of name/value pairs"

nameValPair   :: CharParser a (String,String) 
nameValPair   = do r1 <- itemName 
                   cfws
                   r2 <- itemValue 
                   return (r1,r2)
                  <?> "a name/value pair"

itemName       :: CharParser a String
itemName       = do r1 <- alpha
                    r2 <- many $ choice [ char '-', alpha, digit ]
                    return (r1 : r2)
                  <?> "name of a name/value pair"

itemValue      :: CharParser a String 
itemValue      = choice [ try (do { r <- many1 angleAddr; return (concat r) })  
                        , try addrSpec 
                        , try domain
                        , msgId 
                        , try atom
                        ]
                  <?> "value of a name/value pair"

-- ** Optional fields (section 3.6.8)

-- |Parse an arbitrary header field and return a tuple containing the
-- 'fieldName' and 'unstructured' text of the header. The name will
-- /not/ contain the terminating colon.

optionalField  :: CharParser a (String,String) 
optionalField  = do n <- fieldName 
                    char ':'
                    b <- unstructured
                    crlf
                    return (n,b)
                  <?> "optional (unspecified) header line"

-- |Parse and return an arbitrary header field name. That is one or
-- more 'ftext' characters.

fieldName      :: CharParser a String
fieldName      = many1 ftext <?> "header line name"

-- |Match and return any ASCII character except for control
-- characters, whitespace, and \"@:@\".

ftext           :: CharParser a Char
ftext           = satisfy (\c -> ord c `elem` ([33..57] ++ [59..126]))
                  <?> "character (excluding controls, space, and ':')"


-- * Miscellaneous obsolete tokens (section 4.1)

-- |Match the obsolete \"quoted pair\" syntax, which - unlike
-- 'quotedPair ' - allowed /any/ ASCII character to be specified when
-- quoted. The parser will return both, the backslash and the actual
-- character.

obsQp          :: CharParser a String 
obsQp          = do char '\\' 
                    c <- satisfy (\c -> ord c `elem` [0..127])
                    return ['\\',c]
                 <?> "any quoted US-ASCII character"

-- |Match the obsolete \"text\" syntax, which - unlike 'text' - allowed
-- \"carriage returns\" and \"linefeeds\". This is really weird; you
-- better consult the RFC for details. The parser will return the
-- complete string, including those special characters.

obsText        :: CharParser a String
obsText        = do r1 <- many lf
                    r2 <- many cr
                    r3 <- many (do r4 <- obsChar 
                                   r5 <- many lf
                                   r6 <- many cr
                                   return (r4 : (r5 ++ r6)))
                    return (r1 ++ r2 ++ concat r3)

-- |Match and return the obsolete \"char\" syntax, which - unlike
-- 'character' - did not allow \"carriage return\" and \"linefeed\".

obsChar        :: CharParser a Char 
obsChar        = satisfy (\c -> ord c `elem` ([0..9] ++ [11,12] ++ [14..127])) 
                  <?> "any ASCII character except CR and LF"

-- |Match and return the obsolete \"utext\" syntax, which is identical
-- to 'obsText'.

obsUtext       :: CharParser a String 
obsUtext       = obsText 

-- |Match the obsolete \"phrase\" syntax, which - unlike 'phrase' -
-- allows dots between tokens.

obsPhrase      :: CharParser a [String]
obsPhrase      = do r1 <- word
                    r2 <- many $ choice [ word
                                        , string "."
                                        , do { cfws; return [] }
                                        ]
                    return (r1 : filter (/=[]) r2)

-- |Match a  \"phrase list\" syntax and return the list of 'String's
-- that make up the phrase. In contrast to a 'phrase', the
-- 'obsPhraseList' separates the individual words by commas. This 
-- syntax is - as you will have guessed - obsolete.

obsPhraseList :: CharParser a [String] 
obsPhraseList = do r1 <- many1 (do r <- option [] phrase 
                                   unfold $ char ','
                                   return (filter (/=[]) r))
                   r2 <- option [] phrase
                   return (concat r1 ++ r2)
                <|> phrase


-- * Obsolete folding white space (section 4.2)

-- |Parse and return an \"obsolete fws\" token. That is at least one
-- 'wsp' character, followed by an arbitrary number (including zero)
-- of 'crlf' followed by at least one more 'wsp' character.

obsFws         :: CharParser a String 
obsFws         = do r1 <- many1 wsp 
                    r2 <- many (do r3 <- crlf
                                   r4 <- many1 wsp
                                   return (r3 ++ r4))
                    return (r1 ++ concat r2)


-- * Obsolete Date and Time (section 4.3)

-- |Parse a 'dayName' but allow for the obsolete folding syntax.

obsDayOfWeek :: CharParser a Day  
obsDayOfWeek = unfold dayName <?> "day-of-the-week name"  

-- |Parse a 'year' but allow for a two-digit number (obsolete) and the
-- obsolete folding syntax.

obsYear        :: CharParser a Int 
obsYear        = unfold (do r <- manyN 2 digit 
                            return (normalize (read r :: Int)))
                  <?> "year"
    where
    normalize n
        | n <= 49   = 2000 + n
        | n <= 999  = 1900 + n
        | otherwise = n

-- |Parse a 'monthName' but allow for the obsolete folding syntax.

obsMonth       :: CharParser a Month 
obsMonth       = between cfws cfws monthName <?> "month name" 

-- |Parse a 'day' but allow for the obsolete folding syntax.

obsDay         :: CharParser a Int 
obsDay         = unfold day <?> "day" 

-- |Parse a 'hour' but allow for the obsolete folding syntax.

obsHour        :: CharParser a Int 
obsHour        = unfold hour <?> "hour" 

-- |Parse a 'minute' but allow for the obsolete folding syntax.

obsMinute      :: CharParser a Int 
obsMinute      = unfold minute <?> "minute" 

-- |Parse a 'second' but allow for the obsolete folding syntax.

obsSecond      :: CharParser a Int 
obsSecond      = unfold second <?> "second" 

-- |Match the obsolete zone names and return the appropriate offset.

obsZone        :: CharParser a Int 
obsZone        = choice [ mkZone "UT"  0 
                         , mkZone "GMT" 0
                         , mkZone "EST" (-5)
                         , mkZone "EDT" (-4)
                         , mkZone "CST" (-6)
                         , mkZone "CDT" (-5)
                         , mkZone "MST" (-7)
                         , mkZone "MDT" (-6)
                         , mkZone "PST" (-8)
                         , mkZone "PDT" (-7)
                         , do { r <- oneOf ['A'..'I']; return $ (ord r - 64) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['K'..'M']; return $ (ord r - 65) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['N'..'Y']; return $ -(ord r - 77) * 60*60 } <?> "military zone spec"
                         , do { char 'Z'; return 0 }                                    <?> "military zone spec"
                         ]
    where mkZone n o  = try $ do { string n; return (o*60*60) }


-- * Obsolete Addressing (section 4.4)

-- |This parser will match the \"obsolete angle address\" syntax. This
-- construct used to be known as a \"route address\" in earlier RFCs.
-- There are two differences between this construct and the
-- 'angleAddr': For one - as usual -, the obsolete form allows for 
-- more liberal insertion of folding whitespace and comments.
--
-- Secondly, and more importantly, angle addresses used to allow the
-- (optional) specification of a \"route\". The newer version does not.
-- Such a routing address looks like this:
--
-- >    <@example1.org,@example2.org:simons@example.org>
--
-- The parser will return a tuple that - in case of the above address -
-- looks like this:
--
-- >    (["example1.org","example2.org"],"simons@example.org")
--
-- The first part contains a list of hosts that constitute the route
-- part. This list may be empty! The second part of the tuple is the
-- actual 'addrSpec' address. 

obsAngleAddr  :: CharParser a String  
obsAngleAddr  = unfold (do char '<'  
                           _ <- option [] obsRoute
                           addr <- addrSpec 
                           char '>'
                           return addr)  -- TODO: route is lost here.
                  <?> "obsolete angle address"

-- |This parser parses the \"route\" part of 'obsAngleAddr' and  
-- returns the list of 'String's that make up this route. Relies on
-- 'obsDomainList' for the actual parsing.  

obsRoute       :: CharParser a [String]
obsRoute       = unfold (do { r <- obsDomainList; char ':'; return r })  
                  <?> "route of an obsolete angle address"

-- |This parser parses a list of domain names, each of them prefaced
-- with an \"at\". Multiple names are separated by a comma. The list of
-- 'domain's is returned - and may be empty.

obsDomainList :: CharParser a [String]  
obsDomainList = do char '@'  
                   r1 <- domain
                   r2 <- many (do cfws <|> string ","
                                  optional cfws
                                  char '@'
                                  domain)
                   return (r1 : r2)
                   <?> "route of an obsolete angle address"

-- |Parse the obsolete syntax of a 'localPart', which allowed for
-- more liberal insertion of folding whitespace and comments. The
-- actual string is returned.

obsLocalPart  :: CharParser a String 
obsLocalPart  = do r1 <- word 
                   r2 <- many (do string "."
                                  r <- word
                                  return ('.' : r))
                   return (r1 ++ concat r2)
                 <?> "local part of an address"

-- |Parse the obsolete syntax of a 'domain', which allowed for more
-- liberal insertion of folding whitespace and comments. The actual
-- string is returned.

obsDomain      :: CharParser a String 
obsDomain      = do r1 <- atom 
                    r2 <- many (do string "."
                                   r <- atom
                                   return ('.' : r))
                    return (r1 ++ concat r2)
                  <?> "domain part of an address"

-- |This parser will match the obsolete syntax for a 'mailboxList'. 
-- This one is quite weird: An 'obsMboxList' contains an arbitrary  
-- number of 'mailbox'es - including none -, which are separated by
-- commas. But you may have multiple consecutive commas without giving
-- a 'mailbox'. You may also have a valid 'obsMboxList' that  
-- contains /no/ 'mailbox' at all. On the other hand, you /must/ have
-- at least one comma.
--
-- So, this input is perfectly valid:
--
-- >    ","
--
-- But this one is - contrary to all intuition - not:
--
-- >    "simons@example.org"
--
-- Strange, isn't it?

obsMboxList   :: CharParser a [NameAddr]  
obsMboxList   = do r1 <- many1 (try (do r <- maybeOption mailbox  
                                        unfold $ char ','
                                        return r))
                   r2 <- maybeOption mailbox
                   return [x | Just x <- r1 ++ [r2]]
                 <?> "obsolete syntax for a list of mailboxes"

-- |This parser is identical to 'obsMboxList' but parses a list of  
-- 'address'es rather than 'mailbox'es. The main difference is that an
-- 'address' may contain 'group's. Please note that as of now, the
-- parser will return a simple list of addresses; the grouping
-- information is lost.

obsAddrList   :: CharParser a [NameAddr]  
obsAddrList   = do r1 <- many1 (try (do r <- maybeOption address  
                                        optional cfws
                                        char ','
                                        optional cfws
                                        return r))
                   r2 <- maybeOption address
                   return (concat [x | Just x <- r1 ++ [r2]])
                 <?> "obsolete syntax for a list of addresses"


-- * Obsolete header fields (section 4.5)

obsFields      :: GenParser Char a [Field] 
obsFields      = many (    try (do { r <- obsFrom; return (From r) }) 
                        <|> try (do { r <- obsSender; return (Sender r) }) 
                        <|> try (do { r <- obsReturn; return (ReturnPath r) })
                        <|> try (do { r <- obsReplyTo; return (ReplyTo r) })
                        <|> try (do { r <- obsTo; return (To r) })
                        <|> try (do { r <- obsCc; return (Cc r) }) 
                        <|> try (do { r <- obsBcc; return (Bcc r) }) 
                        <|> try (do { r <- obsMessageId; return (MessageID r) })  
                        <|> try (do { r <- obsInReplyTo; return (InReplyTo r) })  
                        <|> try (do { r <- obsReferences; return (References r) })
                        <|> try (do { r <- obsSubject; return (Subject r) }) 
                        <|> try (do { r <- obsComments; return (Comments r) }) 
                        <|> try (do { r <- obsKeywords; return (Keywords [r]) }) 
                        <|> try (do { r <- obsOrigDate; return (Date r) }) 
                        <|> try (do { r <- obsResentDate; return (ResentDate r) }) 
                        <|> try (do { r <- obsResentFrom; return (ResentFrom r) }) 
                        <|> try (do { r <- obsResentSend; return (ResentSender r) }) 
                        <|> try (do { r <- obsResentTo; return (ResentTo r) })
                        <|> try (do { r <- obsResentCc; return (ResentCc r) }) 
                        <|> try (do { r <- obsResentBcc; return (ResentBcc r) }) 
                        <|> try (do { r <- obsResentMid; return (ResentMessageID r) }) 
                        <|> try (do { r <- obsResentReply; return (ResentReplyTo r) }) 
                        <|> try (do { r <- obsReceived; return (ObsReceived r) })
                         -- catch all
                        <|> (do { (name,cont) <- obsOptional; return (OptionalField name cont) })
                       )


-- ** Obsolete origination date field (section 4.5.1)

-- |Parse a 'date' header line but allow for the obsolete
-- folding syntax.

obsOrigDate   :: CharParser a CalendarTime 
obsOrigDate   = obsHeader  "Date" dateTime 


-- ** Obsolete originator fields (section 4.5.2)

-- |Parse a 'from' header line but allow for the obsolete
-- folding syntax.

obsFrom        :: CharParser a [NameAddr] 
obsFrom        = obsHeader  "From" mailboxList  

-- |Parse a 'sender' header line but allow for the obsolete
-- folding syntax.

obsSender      :: CharParser a NameAddr 
obsSender      = obsHeader  "Sender" mailbox 

-- |Parse a 'replyTo' header line but allow for the obsolete
-- folding syntax.

obsReplyTo    :: CharParser a [NameAddr]
obsReplyTo    = obsHeader  "Reply-To" mailboxList 


-- ** Obsolete destination address fields (section 4.5.3)

-- |Parse a 'to' header line but allow for the obsolete
-- folding syntax.

obsTo          :: CharParser a [NameAddr]
obsTo          = obsHeader  "To" addressList 

-- |Parse a 'cc' header line but allow for the obsolete
-- folding syntax.

obsCc          :: CharParser a [NameAddr] 
obsCc          = obsHeader  "Cc" addressList  

-- |Parse a 'bcc' header line but allow for the obsolete
-- folding syntax.

obsBcc         :: CharParser a [NameAddr] 
obsBcc         = header "Bcc" (    try addressList  
                                    <|> do { optional cfws; return [] }
                               )


-- ** Obsolete identification fields (section 4.5.4)

-- |Parse a 'messageId' header line but allow for the obsolete 
-- folding syntax.

obsMessageId  :: CharParser a String  
obsMessageId  = obsHeader  "Message-ID" msgId  

-- |Parse an 'inReplyTo' header line but allow for the obsolete 
-- folding and the obsolete phrase syntax.

obsInReplyTo :: CharParser a [String]  
obsInReplyTo = obsHeader  "In-Reply-To" (do r <- many (    do {phrase; return [] }  
                                                     <|> msgId 
                                                      )
                                            return (filter (/=[]) r))

-- |Parse a 'references' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obsReferences  :: CharParser a [String]
obsReferences  = obsHeader  "References" (do r <- many (do { phrase; return [] }
                                                       <|> msgId 
                                                       )
                                             return (filter (/=[]) r))

-- |Parses the \"left part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'localPart'.

obsIdLeft     :: CharParser a String  
obsIdLeft     = localPart <?> "left part of an message ID"  

-- |Parses the \"right part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'domain'.

obsIdRight    :: CharParser a String  
obsIdRight    = domain <?> "right part of an message ID"  



-- ** Obsolete informational fields (section 4.5.5)

-- |Parse a 'subject' header line but allow for the obsolete
-- folding syntax.

obsSubject     :: CharParser a String 
obsSubject     = obsHeader  "Subject" unstructured 

-- |Parse a 'comments' header line but allow for the obsolete
-- folding syntax.

obsComments    :: CharParser a String 
obsComments    = obsHeader  "Comments" unstructured 

-- |Parse a 'keywords' header line but allow for the obsolete
-- folding syntax. Also, this parser accepts 'obsPhraseList'. 

obsKeywords    :: CharParser a [String] 
obsKeywords    = obsHeader  "Keywords" obsPhraseList  


-- ** Obsolete resent fields (section 4.5.6)

-- |Parse a 'resentFrom' header line but allow for the obsolete 
-- folding syntax.

obsResentFrom :: CharParser a [NameAddr] 
obsResentFrom = obsHeader  "Resent-From" mailboxList  

-- |Parse a 'resentSender' header line but allow for the obsolete 
-- folding syntax.

obsResentSend :: CharParser a NameAddr 
obsResentSend = obsHeader  "Resent-Sender" mailbox 

-- |Parse a 'resentDate' header line but allow for the obsolete 
-- folding syntax.

obsResentDate :: CharParser a CalendarTime 
obsResentDate = obsHeader  "Resent-Date" dateTime 

-- |Parse a 'resentTo' header line but allow for the obsolete
-- folding syntax.

obsResentTo   :: CharParser a [NameAddr]
obsResentTo   = obsHeader  "Resent-To" mailboxList 

-- |Parse a 'resentCc' header line but allow for the obsolete 
-- folding syntax.

obsResentCc   :: CharParser a [NameAddr] 
obsResentCc   = obsHeader  "Resent-Cc" mailboxList  

-- |Parse a 'resentBcc' header line but allow for the obsolete 
-- folding syntax.

obsResentBcc  :: CharParser a [NameAddr] 
obsResentBcc  = obsHeader  "Bcc" (    try addressList  
                                    <|> do { optional cfws; return [] }
                                   )

-- |Parse a 'resentMsgId' header line but allow for the obsolete  
-- folding syntax.

obsResentMid  :: CharParser a String 
obsResentMid  = obsHeader  "Resent-Message-ID" msgId  

-- |Parse a @Resent-Reply-To@ header line but allow for the
-- obsolete folding syntax.

obsResentReply :: CharParser a [NameAddr] 
obsResentReply = obsHeader  "Resent-Reply-To" addressList  


-- ** Obsolete trace fields (section 4.5.7)

obsReturn      :: CharParser a String 
obsReturn       = obsHeader  "Return-Path" path

obsReceived    :: CharParser a [(String, String)]
obsReceived     = obsHeader  "Received" nameValList  

-- |Match 'obsAngleAddr'.  

obsPath        :: CharParser a String
obsPath        = obsAngleAddr  

-- |This parser is identical to 'optionalField' but allows the more 
-- liberal line-folding syntax between the \"fieldName\" and the \"field
-- text\".

obsOptional    :: CharParser a (String,String)
obsOptional    = do n <- fieldName
                    many wsp
                    char ':'
                    b <- unstructured
                    crlf
                    return (n,b)
                 <?> "optional (unspecified) header line"
