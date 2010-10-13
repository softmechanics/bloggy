{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Static
import Model
import Data.Object
import Data.Object.Yaml
import Control.Monad
import Data.Time
import Data.Maybe
import Control.Applicative
import Text.Hamlet

-- | Normally we would want to load up entries in this, but I want it to run
-- best on CGI.
data Bloggy = Bloggy
    { bloggyStatic :: Static
    , bloggyApproot :: String
    , bloggyTitle :: String
    }

type Handler = GHandler Bloggy Bloggy

loadBloggy :: IO Bloggy
loadBloggy = do
    so <- join $ decodeFile "settings.yaml"
    m <- fromMapping so :: IO [(String, StringObject)]
    ar <- lookupScalar "approot" m :: IO String
    title <- lookupScalar "title" m
    return Bloggy
        { bloggyStatic = fileLookupDir "static" typeByExt
        , bloggyApproot = ar
        , bloggyTitle = title
        }

mkYesod "Bloggy" [$parseRoutes|
/                  MostRecentEntryR         GET
/entry/#String     EntryR                   GET
/feed              FeedR                    GET
/static            StaticR                  Static bloggyStatic
|]

instance Yesod Bloggy where
    approot = bloggyApproot

getMostRecentEntryR :: Handler ()
getMostRecentEntryR = do
    a <- liftIO loadArchive
    redirect RedirectTemporary $ EntryR $ eiSlug $ head $ snd $ head a

getEntryR :: String -> Handler RepHtml
getEntryR slug = do
    bloggy <- getYesod
    entry <- (liftIO $ loadEntry slug) >>= maybe notFound return
    archive <- liftIO loadArchive
    hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %meta!charset=utf-8
        %title $entryTitle.entry$ :: $bloggyTitle.bloggy$
        %link!rel=stylesheet!href=@stylesheet@
        %link!rel=stylesheet!href=@pandoc@
        %link!rel=alternate!type=application/atom+xml!href=@FeedR@
        <link href='http://fonts.googleapis.com/css?family=Cardo' rel='stylesheet' type='text/css'>
        <link href='http://fonts.googleapis.com/css?family=Droid+Sans' rel='stylesheet' type='text/css'>
        <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
        <script src="http://view.jquery.com/trunk/plugins/treeview/lib/jquery.cookie.js"></script>
        <script src="http://view.jquery.com/trunk/plugins/treeview/jquery.treeview.js"</script>
        %script
            var currentSlug = "$slug$";
            $$(function(){$$("#archives").treeview({persist:"location",collapsed:true,unique:true})});
    %body
        %h1 $bloggyTitle.bloggy$
        #wrapper
            #nav
                %h2 Blogroll
                %ul#blogroll
                    %li
                        %a!href=$yesodweb$ Yesod Web Framework
                    %li
                        %a!href=$photos$ Family Photo Gllery
                    %li
                        %a!href=$github$ Projects on Github
                    %li
                        %a!href=$twitter$ Follow me
                %h2 Archives
                %ul#archives
                    $forall archive month
                        %li
                            $fst.month$
                            %ul
                                $forall snd.month entry
                                    %li
                                        %a!href=@EntryR.eiSlug.entry@
                                            $eiTitle.entry$
            #content
                %h2 $entryTitle.entry$ &mdash; $showDay.entryDate.entry$
                #main-content
                    $entryContent.entry$
                <div id="disqus_thread"></div><script type="text/javascript" src="http://disqus.com/forums/snoyblog/embed.js"></script><noscript><a href="http://disqus.com/forums/snoyblog/?url=ref">View the discussion thread.</a></noscript><a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
            .weight
        #footer
            %p
                Powered by 
                %a!href=$yesodweb$ Yesod Web Framework
        <script type="text/javascript">(function() {var links = document.getElementsByTagName('a');var query = '?';for(var i = 0; i < links.length; i++) {if(links[i].href.indexOf('#disqus_thread') >= 0) { query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&'; } } document.write('<script charset="utf-8" type="text/javascript" src="http://disqus.com/forums/snoyblog/get_num_replies.js' + query + '"></' + 'script>'); })(); </script>
        <script type="text/javascript">var _gaq = _gaq || [];_gaq.push(['_setAccount', 'UA-UA-1434510-6']);_gaq.push(['_trackPageview']);(function() {var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);})();</script>
|]
  where
    stylesheet = StaticR $ StaticRoute ["styl.css"] []
    pandoc = StaticR $ StaticRoute ["pandoc.css"] []
    jquerytools = "http://cdn.jquerytools.org/1.1.2/full/jquery.tools.min.js"
    yesodweb = "http://docs.yesodweb.com/"
    photos = "http://www.snoyman.com/photos/"
    github = "http://github.com/snoyberg"
    twitter = "http://twitter.com/snoyberg"

getFeedR :: Handler RepAtom
getFeedR = do
    archive <- liftIO loadArchive
    b <- getYesod
    let archive' = take 5 $ concatMap snd archive
    entries <- liftIO $ catMaybes <$> mapM (loadEntry . eiSlug) archive'
    atomFeed $ AtomFeed
        { atomTitle = bloggyTitle b
        , atomLinkSelf = FeedR
        , atomLinkHome = MostRecentEntryR
        , atomUpdated = UTCTime (entryDate $ head entries) 0
        , atomEntries = map go entries
        }
  where
    go e = AtomFeedEntry
                { atomEntryLink = EntryR $ entrySlug e
                , atomEntryUpdated = UTCTime (entryDate e) 0
                , atomEntryTitle = entryTitle e
                , atomEntryContent = toHtml $ entryContent e
                }

main :: IO ()
main = loadBloggy >>= basicHandler 3000
