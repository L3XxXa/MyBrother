{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
  ) where

import           Model
import           ReadWrite

import           Control.Applicative              ((<|>))

import qualified Data.Text as Text
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Maybe
import           Data.Text                        hiding (map)
import           System.Environment               (getEnv)
import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser
import           Text.Read                        (readMaybe)


{-This file is for Telegram. There I worked with Telegram-bot-simple library (by Fizruk a.k.a. Nikolai Kudasov). 
He wrote that library during LambdaConf 2018 workshop. There are functions for working with our commands, which we send to this bot. -}

--State of chat between user and bot
data ChatState
  = InsertingIncome
  | InsertingIncomeSavedSource Text
  | InsertingExpense
  | InsertingExpenseSavedSource Text
  | SearchingIncome
  | SearchingExpense
  | CheckingBalance
  | EmptyContent
  deriving (Show, Eq)

data ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

emptyChatModel :: ChatModel
emptyChatModel = ChatModel EmptyContent

--how bot should act 
data Action
  = Empty
  | ActHelp
  | ActBalance
  | ActAddInc
  | ActAddExp
  | ActSearchIncome
  | ActSearchExpense
  | ActMessage Text
  deriving (Show, Read)


incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp
  { botInitialModel = emptyChatModel
  , botAction = flip updateToAction
  , botHandler = updateHandler
  , botJobs = []
  }


updateToAction :: ChatModel -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $
  ActHelp <$ command (pack "help") <|>
  ActBalance <$ command (pack "balance") <|>
  ActAddInc <$ command (pack "income") <|>
  ActAddExp <$ command (pack "expense") <|>
  ActSearchIncome <$ command (pack "incomes") <|>
  ActSearchExpense <$ command (pack "expenses") <|>
  ActMessage <$> plainText <|>
  callbackQueryDataRead

replyString :: String -> BotM ()
replyString str = reply . toReplyMessage . pack $ str

--How bot should act to /help message
helpMessage :: Text
helpMessage =
  (intercalate $ pack "\n") $
  map
    pack
    [ "/help to show this message"
    , "/balance to show balance"
    , "/income to insert income"
    , "/expense to insert expense"
    , "/incomes to show incomes from a source"
    , "/expenses to show expenses to an entity"
    ]

--updating state of the chat
updateHandler :: Action -> ChatModel -> Eff Action ChatModel
updateHandler act model =
  case act of
    Empty -> pure model
    ActHelp ->
      emptyChatModel <# do
        reply . toReplyMessage $ helpMessage
        pure Empty
    ActBalance ->
      emptyChatModel <# do
        liftIO balance >>= replyString . show 
        pure Empty
    ActAddInc ->
      ChatModel InsertingIncome <# do
        replyString "Who gave you the money?"
        pure Empty
    ActAddExp ->
      ChatModel InsertingExpense <# do
        replyString "Who did you give it to?"
        pure Empty
    ActSearchIncome ->
      ChatModel SearchingIncome <# do
        replyString "Who are you looking for?"
        pure Empty
    ActSearchExpense ->
      ChatModel SearchingExpense <# do
        replyString "Who are you looking for?"
        pure Empty
    ActMessage msg -> messageHandler msg model

reasking :: ChatModel -> Eff Action ChatModel
reasking model =
  model <# do
    replyString "Please input numbers."
    pure Empty

--sending message to user
messageHandler :: Text -> ChatModel -> Eff Action ChatModel
messageHandler message model =
  case model of
    ChatModel InsertingExpense ->
      ChatModel (InsertingExpenseSavedSource message) <# do
        replyString "How much is it?"
        pure Empty
    ChatModel (InsertingExpenseSavedSource source) ->
      case (readMaybe $ unpack message :: Maybe Double) of
        Nothing -> reasking model
        Just amount ->
          ChatModel EmptyContent <# do
            _ <- liftIO $ insertExpense source amount
            replyString "Ok, saved!"
            pure Empty
    ChatModel InsertingIncome ->
      ChatModel (InsertingIncomeSavedSource message) <# do
        replyString "How much is it?"
        pure Empty
    ChatModel (InsertingIncomeSavedSource source) ->
      case (readMaybe $ unpack message :: Maybe Double) of
        Nothing -> reasking model
        Just amount ->
          ChatModel EmptyContent <# do
            _ <- liftIO $ insertIncome source amount
            replyString "Ok, saved!"
            pure Empty
    ChatModel SearchingExpense ->
      ChatModel EmptyContent <# do
        expenses <- liftIO $ searchExpenseBySource message
        mapM_ (replyString . show) expenses
        pure Empty
    ChatModel SearchingIncome ->
      ChatModel EmptyContent <# do
        incomes <- liftIO $ searchIncomeBySource message
        mapM_ (replyString . show) incomes
        pure Empty
    otherwise ->
      model <# do
        reply . toReplyMessage $ helpMessage
        pure Empty


--this func starts my bot.

run:: Token -> IO()
run token = do
  env<-defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId incexpBotApp) env

someFunc::IO()
someFunc = do 
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token

