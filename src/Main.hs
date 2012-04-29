--
-- AUTHOR : Philipp Balzarek <p.balzarek@googlemail.com
-- LICENSE: MIT
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Monad(forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans(lift)
import           Data.Maybe(fromJust)
import qualified Data.Text as Text
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Keymap
import           Graphics.UI.Gtk.WidgetBuilder
import           Graphics.UI.Prototyper
import qualified Network.XMPP as XMPP
import qualified Network.XMPP.Message as XMPP
import qualified Network.XMPP.SASL as XMPP

-- import Graphics.UI.Gtk.WidgetBuilder

we :: XMPP.JID
we = read "testuser1@species64739.dyndns.org/bot1"

supervisor :: XMPP.JID
supervisor = read "uart14@species64739.dyndns.org"

autoAccept log = forever $ do
  st <- XMPP.waitForPresence XMPP.isPresenceSubscribe
  let them = fromJust $ XMPP.presenceFrom st
  XMPP.sendPresence $ XMPP.presenceSubscribed them
  XMPP.sendPresence $ XMPP.presenceSubscribe them
  liftIO . log $ "Subscription request from " ++ show them

main = run

startSession sess = do
    p <- ask
    let status = liftIO . flip runReaderT p . setStatus
    liftIO . XMPP.withSession sess $ do
        status "starting"
        XMPP.connect "localhost" "species64739.dyndns.org"
        status "connected, SASL"
        XMPP.startTLS XMPP.exampleParams
        saslResponse <- XMPP.auth (fromJust $ XMPP.localpart we) "pwd" (XMPP.resourcepart we)
        case saslResponse of
            Right _ -> return ()
            Left e -> error $ show e
        status "going online"
        XMPP.sendPresence XMPP.presenceOnline
        status "online"
        XMPP.sendMessage $ XMPP.simpleMessage supervisor "running"

chPresence sess = let statusChoices =
                         [ ("Available" , XMPP.Available )
                         , ("Away"      , XMPP.Away      )
                         , ("FreeChat"  , XMPP.FreeChat  )
                         , ("DND"       , XMPP.DND       )
                         , ("XAway"     , XMPP.XAway     )
                         ]
  in withChoice' statusChoices $ \s ->
     withInput "Status Message" "" $ \txt ->
     liftIO . XMPP.withSession sess $ do
         XMPP.sendPresence $ XMPP.status
             (if null txt then Nothing else Just $ Text.pack txt) (Just s) Nothing

myKeymap sess = mkKeymap [ ((0, "s"), lift . withInput "send message" "" $
                                   \i -> liftIO . XMPP.withSession sess .
                                           XMPP.sendMessage $
                                             XMPP.simpleMessage supervisor
                                               (Text.pack i)
                                             )
                         , ((0,"c"), lift $ do
                                    withChoice' (let choices = map show [1..30] in zip choices choices) (liftIO . putStrLn ))
                         , ((shift, "s"), startSession sess)
                         , ((0 , "p"), lift $ chPresence sess )
                    ]

run = do
    sess <- XMPP.newSession
    flip uiMain (myKeymap sess) $ do
        (w , add) <- createLog
        liftIO . mapM add $ [ "S - connect"
                            , "s - send message"
                            , "p - change presence"
                            ]
        liftIO . XMPP.withSession sess $ do
            XMPP.fork $ autoAccept add
            XMPP.fork . forever $ do
                m <- XMPP.pullMessage
                case m of
                    Left e -> liftIO . add $ "error: " ++ show e
                    Right r -> do
                        let from = maybe "" show (XMPP.messageFrom r)
                        let txt = maybe "<no body>" Text.unpack
                                        (XMPP.messageBody r)
                        liftIO $ add (from ++ " : " ++ txt)

        return $ toWidget w

