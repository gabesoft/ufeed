{-# LANGUAGE OverloadedStrings #-}

-- | Mocks for FeedUpdater tests
module Mocks.FeedUpdater where

import Data.Text (empty)
import Types

modified :: LastModified
modified =
  LastModified
  { etag = Just "21/wgdH00VEA79D67U4iiIn3E2U"
  , lastModified = Just "Sun, 11 Sep 2016 16:36:07 GMT"
  }

feed :: Feed
feed =
  (nullFeed empty)
  { feedAuthor = Nothing
  , feedDate = Just "2016-09-11T16:46:00+00:00"
  , feedDescription =
    Just
      "The extensible, customizable, self-documenting real-time display editor."
  , feedFavicon = Just "https://www.redditstatic.com/icon.png/"
  , feedFormat = Just Atom1
  , feedGuid = Just "/guid/r/emacs/.rss"
  , feedId = Just "566e6d8b13415194b8df8024"
  , feedImage =
    Just
      Image
      { imageTitle = ""
      , imageUrl = "https://f.thumbs.redditmedia.com/1CBmBnoqVxNCXdVS.png"
      }
  , feedLastModified =
    Just
      LastModified
      { etag = Nothing
      , lastModified = Nothing
      }
  , feedLastPostDate = Just "2016-09-11T16:36:34.000Z"
  , feedLastReadDate = Just "2016-09-11T16:45:59.567Z"
  , feedLastReadStatus = Just ReadSuccess
  , feedLink = Just "https://www.reddit.com/r/emacs/alt"
  , feedPostCount = 20
  , feedTitle = "M-x emacs-reddit"
  , feedUri = "https://www.reddit.com/r/emacs/.rss"
  }

posts :: [Post]
posts =
  [ nullPost
    { postAuthor = Just "/u/desipenguin"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>They both seem to have similar purpose. But <code>org-journal</code> looks rich in terms of features. When should I use <code>diary-mode</code> ?</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/desipenguin\"> /u/desipenguin </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/527jg8/what_is_the_difference_between_diary_mode_and/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/527jg8/what_is_the_difference_between_diary_mode_and/\">[comments]</a></span>"
    , postGuid = "t3_527jg8"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/527jg8/what_is_the_difference_between_diary_mode_and/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "What is the difference between diary mode and org-journal (when to use which ?)"
    }
  , nullPost
    { postAuthor = Just "/u/syohex"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p><a href=\"https://github.com/syohex/emacs-go-add-tags\">https://github.com/syohex/emacs-go-add-tags</a></p> <p>go-add-tags is inspired by vim-go&#39;s GoAddTags command. It inserts go struct field tag. This is useful for writing struct definition which is bound JSON, YAML, TOML by unmarshal function. You can install go-add-tags from MELPA or MELPA stable. Please report me via github issues if you have any issues or suggesttions.</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/syohex\"> /u/syohex </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/5289k8/goaddtags_insert_golang_struct_field_tag/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/5289k8/goaddtags_insert_golang_struct_field_tag/\">[comments]</a></span>"
    , postGuid = "t3_5289k8"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/5289k8/goaddtags_insert_golang_struct_field_tag/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "go-add-tags: Insert Golang struct field tag"
    }
  , nullPost
    { postAuthor = Just "/u/sam217pa"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "&#32; submitted by &#32; <a href=\"https://www.reddit.com/user/sam217pa\"> /u/sam217pa </a> <br/> <span><a href=\"https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/529bzc/nuclear_weapon_multiediting_via_ivy_and_ag/\">[comments]</a></span>"
    , postGuid = "t3_529bzc"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/529bzc/nuclear_weapon_multiediting_via_ivy_and_ag/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Nuclear weapon multi-editing via Ivy and Ag"
    }
  , nullPost
    { postAuthor = Just "/u/SugaaH"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>In helm-bibtex, is there a way to directly go to the BibTeX entry in the corresponding bib file from the dropdown? This post (<a href=\"https://nickhigham.wordpress.com/2016/01/06/managing-bibtex-files-with-emacs/\">https://nickhigham.wordpress.com/2016/01/06/managing-bibtex-files-with-emacs/</a>) says &quot;Hitting tab displays several options, which include options to insert a \\cite command for the item under the cursor, to insert a formatted reference for the item under the cursor, and to go to that entry in the bib file. &quot; But hitting &quot;tab&quot; doesn&#39;t do anything for me. </p> <p>Any hint is appreciated. For example, if I want to jump to the source bib file for the following yoganarasimhan reference</p> <p><a href=\"http://imgur.com/a/2gAUi\">http://imgur.com/a/2gAUi</a></p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/SugaaH\"> /u/SugaaH </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/526tpv/helmbibtex_question_how_to_go_to_the_bibtex_entry/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/526tpv/helmbibtex_question_how_to_go_to_the_bibtex_entry/\">[comments]</a></span>"
    , postGuid = "t3_526tpv"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/526tpv/helmbibtex_question_how_to_go_to_the_bibtex_entry/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "helm-bibtex question: how to go to the BibTeX entry in the bib file from the dropdown"
    }
  , nullPost
    { postAuthor = Just "/u/csrtwister"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I want to start using emacs for writing python code and I already have setup <code>anaconda-mode</code> and <code>company-anaconda</code>. But, I see that there is a package in MELPA called <code>python-mode</code> which is a major mode for editing python files. But, the author of the package have not added any <code>readme</code> to highlight its features nor has he added a wiki page to explain what the package does. </p> <p>What does the <code>python-mode</code> package offer over the inbuilt python major mode in emacs ? Should I install this package?</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/csrtwister\"> /u/csrtwister </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/5274w8/about_pythonmode_in_melpa/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/5274w8/about_pythonmode_in_melpa/\">[comments]</a></span>"
    , postGuid = "t3_5274w8"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/5274w8/about_pythonmode_in_melpa/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "About `python-mode` in MELPA"
    }
  , nullPost
    { postAuthor = Just "/u/redguardtoo"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p><a href=\"https://github.com/redguardtoo/elpa-mirror\">https://github.com/redguardtoo/elpa-mirror</a></p> <p>Create local Emacs package repository.</p> <p>Change Log:</p> <ul> <li>support Windows</li> </ul> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/redguardtoo\"> /u/redguardtoo </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/521uyh/elpamirror_200/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/521uyh/elpamirror_200/\">[comments]</a></span>"
    , postGuid = "t3_521uyh"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/521uyh/elpamirror_200/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "elpa-mirror 2.0.0"
    }
  , nullPost
    { postAuthor = Just "/u/Eldrik"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I know that spacemacs has that feature: You type M-x blah and then if blah is bound to a key, it shows the key that blah is bound to. What mode/plugin does that? Thanks in advance.</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/Eldrik\"> /u/Eldrik </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/5214pi/i_type_mx_blah_how_to_know_if_blah_is_bound_to_a/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/5214pi/i_type_mx_blah_how_to_know_if_blah_is_bound_to_a/\">[comments]</a></span>"
    , postGuid = "t3_5214pi"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/5214pi/i_type_mx_blah_how_to_know_if_blah_is_bound_to_a/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "I type M-x blah. how to know if blah is bound to a key"
    }
  , nullPost
    { postAuthor = Just "/u/SugaaH"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>have a question about emacs latex. </p> <p>Is there a way when you enter \\cite{ in the Tex document, a helm/ivy reference window opens that you can search for reference and enter to insert?</p> <p>Currently I use C-c C-[, which is quite slower than sublime text (latextools plugin: whenever you write \\cite{, \\ref{, etc., the command pallet will prompt a drag down in which you can search for references and enter to insert) </p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/SugaaH\"> /u/SugaaH </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51zosz/prompt_for_reference_when_entering_cite/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51zosz/prompt_for_reference_when_entering_cite/\">[comments]</a></span>"
    , postGuid = "t3_51zosz"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51zosz/prompt_for_reference_when_entering_cite/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Prompt for reference when entering \\cite{"
    }
  , nullPost
    { postAuthor = Just "/u/mC_mC_mC_"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Much like org-mode, in markdown-mode I can no longer use &lt;M-left&gt; and &lt;M-right&gt; to navigate through text. Instead of left-word and right-word, it now calls markdown-promote and markdown-demote.<br/> How can I change this?<br/> Thanks </p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/mC_mC_mC_\"> /u/mC_mC_mC_ </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51zyda/markdown_mode_hijacked_leftword_and_rightword/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51zyda/markdown_mode_hijacked_leftword_and_rightword/\">[comments]</a></span>"
    , postGuid = "t3_51zyda"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51zyda/markdown_mode_hijacked_leftword_and_rightword/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Markdown mode hijacked left-word and right-word"
    }
  , nullPost
    { postAuthor = Just "/u/stunpix"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Hi guys. I&#39;m running emacs on linux laptop with HiDPI display. I&#39;ve tuned my environment (X and fonts) for this display and all apps are looking nice (emacs also), but Flycheck wavy underlines and error marks are really tiny. This is the only problem. Not fonts, not menus, only underlines and error marks on left bar. Anyone faced same issue? How you fixed it?</p> <p>I suppose marks and underlines are bitmaps and emacs should be patched with new ones to support HiDPI displays.</p> <p>PS: I already tried to change styles for underlines, but they have no scaling/size params.</p> <p>UPD: here is how it looks <a href=\"https://i.imgur.com/hJ8WiNy.png\">https://i.imgur.com/hJ8WiNy.png</a></p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/stunpix\"> /u/stunpix </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51wm7d/emacs_on_hidpi_displays/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51wm7d/emacs_on_hidpi_displays/\">[comments]</a></span>"
    , postGuid = "t3_51wm7d"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51wm7d/emacs_on_hidpi_displays/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Emacs on HiDPI displays?"
    }
  , nullPost
    { postAuthor = Just "/u/jpf137"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "&#32; submitted by &#32; <a href=\"https://www.reddit.com/user/jpf137\"> /u/jpf137 </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51y8ri/is_there_a_package_to_make_latex_includeinput_and/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51y8ri/is_there_a_package_to_make_latex_includeinput_and/\">[comments]</a></span>"
    , postGuid = "t3_51y8ri"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51y8ri/is_there_a_package_to_make_latex_includeinput_and/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "Is there a package to make latex \"include/input\" and python functions/imports clickable (go to file where function was defined/chapter file)?"
    }
  , nullPost
    { postAuthor = Just "/u/prasannarajaram"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Today I installed the Ubuntu based Bash shell in Windows 10 (after the 1607 or &quot;anniversary update&quot;. </p> <p>I installed <code>emacs</code> via the <code>apt-get</code>. I have the X11 version of Emacs. I&#39;m conversant with using emacs in GUI mode but I have never used in X11 mode. </p> <p>I want to know how I could increase the font size (read font scaling). I tried using the <code>C-x C-+</code> command in vain. </p> <p>How can I increase the font size?</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/prasannarajaram\"> /u/prasannarajaram </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51yumn/ubuntu_based_bash_shell_for_windows_10_emacs/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51yumn/ubuntu_based_bash_shell_for_windows_10_emacs/\">[comments]</a></span>"
    , postGuid = "t3_51yumn"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51yumn/ubuntu_based_bash_shell_for_windows_10_emacs/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just "Ubuntu based Bash Shell for Windows 10 - Emacs installation"
    }
  , nullPost
    { postAuthor = Just "/u/wizmer"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Hello guys !</p> <p>I am super happy to announce that I have dropped my first APK release on Github. It is the first iteration of what I called MobileOrg 2.0, an Android Org Mode parser with a Git synchronization. (For the time being I have disabled all other sync mechanism as I want to debug the Git sync but they will be re-enabled in the future)</p> <p>Besides the fact it uses Git, the major change is the fact than MobileOrg is now standalone. It means you no longer have to interact and use weird commands such as M-x org-mobile-push to synchronize. This implies that the app has to create the agenda file by its own (I am aware the agenda is not functional for the time being).</p> <p>Quick Start:</p> <ul> <li><p>Go <a href=\"https://github.com/matburt/mobileorg-android/releases/tag/2.0\">here</a> and download the APK</p></li> <li><p>Put the APK on your phone and click it to install it (you&#39;ll have to allow unsigned APK to continue)</p></li> <li><p>When you&#39;ll launch the app for the first time you&#39;ll be asked Git credentials. Here are a 2 examples of how to fill the stuff.</p></li> <li><p>Sync with a github repo (let&#39;s take mine at <a href=\"https://github.com/wizmer/OrgNotes\">https://github.com/wizmer/OrgNotes</a>) As MobileOrg does not support (yet) https, you&#39;ll have to use SSH+private key authentication. In username set: <strong>git</strong> (not your username !!), select your private key, in absolute path set: wizmer/OrgNotes<strong>.git</strong> (don&#39;t forget the .git !) and in host: github.com, you can let port blank as port 22 is default.</p></li> <li><p>Now to sync with you own repo on your own server. Filling the form is straightforward. Just make sure to use the <strong>absolute</strong> path. But the caveat is that your repository must be in <strong>bare</strong> mode. Here is a procedure to create a bare repo with the org file you want to synchronize.</p></li> </ul> <ol> <li><p>Go on your server</p></li> <li><p>Create the bare repo: mkdir bareRepo &amp;&amp; cd bareRepo &amp;&amp; git init --bare</p></li> <li><p>Clone it: cd .. &amp;&amp; git clone bareRepo nonBare</p></li> <li><p>you will use the nonBare to commit and push your first files: cp *.org nonBare/ &amp;&amp; cd nonBare &amp;&amp; git add *.org &amp;&amp; git commit -m &quot;adding my files&quot; &amp;&amp; git push</p></li> <li><p>you can now delete the nonBare repo: cd .. &amp;&amp; rm -rf nonBare</p></li> <li><p>back to MobileOrg and specify the absolute path to the bare repo</p></li> </ol> <p>Enjoy !</p> <p>PS: And don&#39;t forget to give me feedbacks !! You can use the <a href=\"https://github.com/matburt/mobileorg-android/issues\">Github issue tracker</a></p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/wizmer\"> /u/wizmer </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51tqkb/android_mobileorg_20_call_for_tester/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51tqkb/android_mobileorg_20_call_for_tester/\">[comments]</a></span>"
    , postGuid = "t3_51tqkb"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51tqkb/android_mobileorg_20_call_for_tester/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Android MobileOrg 2.0 Call for tester !!"
    }
  , nullPost
    { postAuthor = Just "/u/chmouelb"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "&#32; submitted by &#32; <a href=\"https://www.reddit.com/user/chmouelb\"> /u/chmouelb </a> <br/> <span><a href=\"http://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51sflk/dealing_with_yaml_in_emacs/\">[comments]</a></span>"
    , postGuid = "t3_51sflk"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51sflk/dealing_with_yaml_in_emacs/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Dealing with yaml in Emacs"
    }
  , nullPost
    { postAuthor = Just "/u/enki"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "&#32; submitted by &#32; <a href=\"https://www.reddit.com/user/enki\"> /u/enki </a> <br/> <span><a href=\"https://www.reddit.com/r/lisp/comments/4z3d5p/femtoemacs_tiny_emacs_clone_with_configuring_in/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51ts79/femtoemacs_tiny_emacs_clone_with_configuring_in/\">[comments]</a></span>"
    , postGuid = "t3_51ts79"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51ts79/femtoemacs_tiny_emacs_clone_with_configuring_in/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "Femto-Emacs - Tiny emacs clone with configuring in FemtoLisp \8226 /r/lisp"
    }
  , nullPost
    { postAuthor = Just "/u/NLisa"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I&#39;ve been using Emacs daily for the past ~18 months, for everything from C/C++ and Python Programming, editting wikipages, preparing journal articles in LaTeX, maintaining remote servers with Tramp and generally organising my life in org-mode.</p> <p>I have been completely enthralled with the Emacsen way of doing things, from syncing all my org-files on my phone with mobile-org, to adopting Emacs keybindings in terminals, to using Conkeror as me default browser.</p> <p>I understand that the vast majority of proponents of Evil-Mode / &quot;un-Holy&quot; Spacemacs are &#39;rehabilitated&#39; vim-users who have migrated to Emacs. I would like to know from primarily Emacs users that have migrated to Vim-bindings, have they noticed a productivity benefit in doing so?</p> <p>My knowledge and useage of Vim, is limited to basic sysadmin tasks on barebones servers, i.e. editing config files in vi. So I have an understanding of Vi/Vim modals etc, but would it be considered a worth-while time-investment, migrating to the unholy dark side?</p> <p>Thank you.</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/NLisa\"> /u/NLisa </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51s63h/will_there_be_any_productivity_benefits_for_an/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51s63h/will_there_be_any_productivity_benefits_for_an/\">[comments]</a></span>"
    , postGuid = "t3_51s63h"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51s63h/will_there_be_any_productivity_benefits_for_an/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "Will There Be Any Productivity Benefits for An Emacs User Adopting Evil-Mode \\ Un-Holy Spacemacs"
    }
  , nullPost
    { postAuthor = Just "/u/Eldrik"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I am working behind a proxy so I cannot use melpa fluently directly from my init.el. However, if I open a browser and manually download a dependency, then it works. How can I use this trick together with use package to bootstrap my emacs config behind a proxy? Do I put everything in a folder and set up loadpath? (obs: even this does not work: <a href=\"https://www.emacswiki.org/emacs/InstallingPackages#toc3\">https://www.emacswiki.org/emacs/InstallingPackages#toc3</a>)</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/Eldrik\"> /u/Eldrik </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51sixp/working_behind_a_proxy_how_do_i_manually_install/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51sixp/working_behind_a_proxy_how_do_i_manually_install/\">[comments]</a></span>"
    , postGuid = "t3_51sixp"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51sixp/working_behind_a_proxy_how_do_i_manually_install/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "working behind a proxy. how do I manually install dependencies from melpa"
    }
  , nullPost
    { postAuthor = Just "/u/prasannarajaram"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I&#39;m creating an <code>elisp</code> file based on <a href=\"http://ergoemacs.org/emacs/elisp_syntax_coloring.html\">http://ergoemacs.org/emacs/elisp_syntax_coloring.html</a> from this site. </p> <p>My updated code is shown below &lt;!-- language: elisp --&gt; </p> <pre><code>;;; MOD5-mode.el --- sample major mode for editing MOD5 code. ;; Copyleft \169 2016, by ;; Author: (@gmail.com) ;; Version: 0.0.1 ;; Created: 07-Sep-2016 ;; Keywords: languages ;; Homepage: n/a ;; This file is not part of GNU Emacs. ;;; License: ;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2. ;;; Commentary: ;; This elisp file will enable the highlight of the MOD5 code when viewed in Emacs. ;; ********* probably a link will be helpful here - under construction *********** ;;; Code: ;; define several category of keywords (setq MOD5-keywords &#39;(&quot;SEQ_SKEL&quot; &quot;NEMG&quot; &quot;NSDN&quot; &quot;SSTEP&quot; &quot;STEP&quot; &quot;START&quot; &quot;SCU&quot; &quot;LAMP&quot; &quot;HORN&quot; &quot;PRINTER&quot; &quot;MODULE&quot; &quot;END MODULE&quot; &quot;CALL TEMPLATE&quot; &quot;IF&quot; &quot;FOR&quot;) ) (setq MOD5-types &#39;(&quot;AI&quot; &quot;AO&quot; &quot;DI&quot; &quot;DO&quot; &quot;AISIM&quot; &quot;DISIM&quot; &quot;ACO&quot; &quot;AIM&quot; &quot;AOT&quot; &quot;DOT&quot; &quot;AP&quot; &quot;AC&quot; &quot;DC&quot; &quot;DT&quot; &quot;DM&quot; &quot;AK&quot; &quot;AG&quot; &quot;AR&quot; &quot;TA&quot;)) (setq MOD5-constants &#39;(&quot;PFS&quot; &quot;NFS&quot; &quot;IONE&quot; &quot;ZERO&quot;)) (setq MOD5-events &#39;(&quot;WARN&quot; &quot;REQ&quot; &quot;ALT&quot;)) (setq MOD5-functions &#39;(&quot;ALM_DI_DO05B&quot; &quot;IFD03B&quot; &quot;ALM_4S_AX04B&quot; &quot;ALM_DX05B&quot; &quot;ALM_RERING03B&quot; &quot;ALM_DI_DO05B&quot; &quot;IS04B&quot;)) (setq MOD5-operators &#39;(&quot;=&quot; &quot;AND&quot; &quot;OR&quot; &quot;XOR&quot;)) (setq MOD5-acmdb &#39;(&quot;\\[(&amp;A-Z0-9_)\\]+&quot; &quot;\\[(:A-Z0-9_)\\]&quot;)) ;; generate regex string for each category of keywords (setq MOD5-keywords-regexp (regexp-opt MOD5-keywords &#39;words)) (setq MOD5-type-regexp (regexp-opt MOD5-types &#39;words)) (setq MOD5-constant-regexp (regexp-opt MOD5-constants &#39;words)) (setq MOD5-event-regexp (regexp-opt MOD5-events &#39;words)) (setq MOD5-functions-regexp (regexp-opt MOD5-functions &#39;words)) (setq MOD5-operators-regexp (regexp-opt MOD5-operators &#39;words)) (setq MOD5-acmdb-regexp (regexp-opt MOD5-acmdb &#39;words)) ;; create the list for font-lock. ;; each category of keyword is given a particular face (setq MOD5-font-lock-keywords `( (,MOD5-type-regexp . font-lock-type-face) (,MOD5-constant-regexp . font-lock-constant-face) (,MOD5-event-regexp . font-lock-builtin-face) (,MOD5-functions-regexp . font-lock-function-name-face) (,MOD5-keywords-regexp . font-lock-keyword-face) (,MOD5-acmdb-regexp . font-lock-variable-name-face) (,MOD5-operators-regexp . font-lock-negation-char-face) ;; note: order above matters, because once colored, that part won&#39;t change. ;; in general, longer words first )) ;;;###autoload (define-derived-mode MOD5-mode fundamental-mode &quot;MOD5 mode&quot; &quot;Major mode for editing MOD5 Dowtran Code\8230&quot; ;; code for syntax highlighting (setq font-lock-defaults &#39;((MOD5-font-lock-keywords)))) ;; clear memory. no longer needed (setq MOD5-keywords nil) (setq MOD5-types nil) (setq MOD5-constants nil) (setq MOD5-events nil) (setq MOD5-functions nil) (setq MOD5-acmdb nil) (setq MOD5-operators nil) ;; clear memory. no longer needed (setq MOD5-keywords-regexp nil) (setq MOD5-types-regexp nil) (setq MOD5-constants-regexp nil) (setq MOD5-events-regexp nil) (setq MOD5-functions-regexp nil) (setq MOD5-acmdb-regexp nil) (setq MOD5-operators-regexp nil) ;; add the mode to the `features&#39; list (provide &#39;MOD5-mode) ;; Local Variables: ;; coding: utf-8 ;; End: ;;; MOD5-mode.el ends here </code></pre> <p>In the above code, I&#39;m unable to get the following to work: </p> <pre><code>(setq MOD5-operators &#39;(&quot;=&quot; &quot;AND&quot; &quot;OR&quot; &quot;XOR&quot;)) (setq MOD5-acmdb &#39;(&quot;\\[(&amp;A-Z0-9_)\\]+&quot; &quot;\\[(:A-Z0-9_)\\]&quot;)) </code></pre> <p>What is wrong here? I have copy-pasted the existing code and created the above two lines and for some reason they do not highlight what is within quotes. </p> <p>And also the regex <code>&quot;\\[(&amp;A-Z0-9_)\\]+&quot; &quot;\\[(:A-Z0-9_)\\]&quot;</code> is intended to match text like </p> <ol> <li>:ALM_SIP_PROB_123</li> <li>(:ALM_SIP_PROB_123)</li> <li>(&amp;:ALM_SIP_PROB_123)<br/></li> <li>PIT14501</li> </ol> <p>I&#39;m using the following to generate the regex string: </p> <pre><code>(setq MOD5-operators-regexp (regexp-opt MOD5-operators &#39;words)) (setq MOD5-acmdb-regexp (regexp-opt MOD5-acmdb &#39;words)) </code></pre> <p>I&#39;m wondering if something could be wrong with this? </p> <p><strong>Update</strong> - Adding a sample program </p> <p><a href=\"http://i.stack.imgur.com/ePW93.png\">!Snapshot of program</a> </p> <p>From the above image you can see that the <code>OR</code>, <code>AND</code> have not been highlighted.<br/> Also the numbers within parenthesis have not been highlighted </p> <p><strong>Update</strong> - Changed the elisp code - face name updated from <code>font-lock-operator</code> to <code>font-lock-negation-char-face</code> </p> <pre><code>(,MOD5-operators-regexp . font-lock-negation-char-face) </code></pre> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/prasannarajaram\"> /u/prasannarajaram </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51rklg/syntax_highlighting_using_emacs/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51rklg/syntax_highlighting_using_emacs/\">[comments]</a></span>"
    , postGuid = "t3_51rklg"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51rklg/syntax_highlighting_using_emacs/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Syntax highlighting using Emacs"
    }
  , nullPost
    { postAuthor = Just "/u/zreeon"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Hi all - </p> <p>I was wondering if there is a blog post somewhere comparing helm with ivy? I&#39;ve been using helm for a while, but ivy looks neat and I was hoping to see how they compare without going through the hassle of setting ivy...</p> <p>Or what&#39;s your own experience with them?</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/zreeon\"> /u/zreeon </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/\">[comments]</a></span>"
    , postGuid = "t3_51lqn9"
    , postLink = "https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Helm or ivy?"
    }
  , nullPost
    { postAuthor = Just "/u/abhinavrk"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>tl;dr</p> <p>Emacs noob. SublimeText adept. Wants to get started with Emacs but wants a version thats more like Sublime (possible modifications to Spacemacs), with system-wide default behavior (C-c, C-x, C-v, etc).</p> <p>Cheers</p> <hr/> <p>Hey All,</p> <p>I mainly work in java, python, javascript (HTML and CSS included) and Clojure (Ruby, Bash, and C to a lesser extent).</p> <p>I usually use eclipse for java (I&#39;m not trying to use that) but end up using sublime for the other stuff (sometimes ATOM).</p> <p>I have an entire workflow built around sublime (and ofc Ctrl-C, Ctrl-V... I usually have a bunch of code I&#39;ve written in the past lying around and tend to copy and paste functions as needed, especially if it&#39;s a one time thing. DRY has bit me in the ass too many times for me to believe in it).</p> <p>My main problem with Sublime is that I have no real access to a shell, but the rest of it is really user friendly.</p> <p>I wanna move to emacs (1) because it&#39;s open source and (2) because I can get a shell from within emacs (great for those ssh sessions and stuff where you&#39;re copying in stuff).</p> <p>While I&#39;m willing to invest some time into learning some key bindings, I&#39;m mainly interested in hitting the ground running. I&#39;m currently looking at Spacemacs (I got some vim knowledge) but what I really hate is the unintended side effects.</p> <p>I was wondering if I could make emacs more like sublime text in that there are a small set of commands that work (C-c, C-x, C-v, etc), and the rest are accessible via the command pallet and some fuzzy searching.</p> <p>Thanks,</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/abhinavrk\"> /u/abhinavrk </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51p3hf/emacs_with_familiar_keybindings/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51p3hf/emacs_with_familiar_keybindings/\">[comments]</a></span>"
    , postGuid = "t3_51p3hf"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51p3hf/emacs_with_familiar_keybindings/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Emacs with familiar keybindings?"
    }
  , nullPost
    { postAuthor = Just "/u/Eldrik"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>I use spacemacs and it&#39;s all fun and games. but I&#39;m trying to build my own config to have a better understanding of the internals. As I understand use-package is integrated inside emacs as of version 24. But el-get is a runner up that is mentioned all the time. What do you use? What should a begginer use? </p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/Eldrik\"> /u/Eldrik </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51mbpi/starting_out_own_emacs_config_do_i_use_usepackage/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51mbpi/starting_out_own_emacs_config_do_i_use_usepackage/\">[comments]</a></span>"
    , postGuid = "t3_51mbpi"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51mbpi/starting_out_own_emacs_config_do_i_use_usepackage/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "starting out own emacs config. Do I use use-package or el-get? thoughts?"
    }
  , nullPost
    { postAuthor = Just "/u/DavidWindonlight"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Hi,</p> <p>I have been using Emacs for over 2 years and the Spacemacs configuration since a year on OS X. Recently I made the move to Windows 10 to make life challenging...</p> <p>Installing Emacs was a piece of cake - On the other hand installing Spacemacs has been a disaster. I have reset my computer twice now.</p> <p>I have used the following strategies: - <a href=\"http://emacsbinw64.sourceforge.net/\">http://emacsbinw64.sourceforge.net/</a> - <a href=\"https://www.youtube.com/watch?v=uaoN1rLfP00\">https://www.youtube.com/watch?v=uaoN1rLfP00</a></p> <p>Every time I am hit with the following error when I add php to the .spacemacs file - &quot;Found 1 new package(s) to install... --&gt; refreshing package archive: gnu... [3/3] --&gt; installing package: <a href=\"mailto:php-extras@php\">php-extras@php</a>... [1/1] An error occurred while installing php-extras (error: (wrong-type-argument stringp nil))&quot;</p> <p>Yes I have also deleted the elpa archive folder.</p> <p>I need help...Please give me a step by step process of getting this working.</p> <p>Thanks</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/DavidWindonlight\"> /u/DavidWindonlight </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51ptkl/wrongtypeargument_stringp_nil/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51ptkl/wrongtypeargument_stringp_nil/\">[comments]</a></span>"
    , postGuid = "t3_51ptkl"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51ptkl/wrongtypeargument_stringp_nil/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "(wrong-type-argument stringp nil)"
    }
  , nullPost
    { postAuthor = Just "/u/Zeekawla99ii"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>Is there any difference between using the emacs version to copy/paste, and the keyboard shortcuts &quot;command+V&quot; and &quot;command+C&quot;</p> <p>Sorry for the n00b question, and thanks for any help!</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/Zeekawla99ii\"> /u/Zeekawla99ii </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51o4yn/n00b_question_why_does_one_use_activate_mark_via/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51o4yn/n00b_question_why_does_one_use_activate_mark_via/\">[comments]</a></span>"
    , postGuid = "t3_51o4yn"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51o4yn/n00b_question_why_does_one_use_activate_mark_via/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "n00b question: Why does one use \"Activate Mark\" via C+space in order to copy and paste?"
    }
  , nullPost
    { postAuthor = Just "/u/ispinfx"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>The icons: <a href=\"https://github.com/domtronn/all-the-icons.el\">https://github.com/domtronn/all-the-icons.el</a></p> <p>The author&#39;s config: <a href=\"https://github.com/hlissner/.emacs.d\">https://github.com/hlissner/.emacs.d</a></p> <p>A theme based on the icon package: <a href=\"https://github.com/hlissner/emacs-doom-theme\">https://github.com/hlissner/emacs-doom-theme</a></p> <p>Screenshots: <a href=\"https://github.com/hlissner/.emacs.d/tree/screenshots\">https://github.com/hlissner/.emacs.d/tree/screenshots</a> </p> <p>It seems that there is too much customizations on the <code>mode-line</code> in the author&#39;s emacs config but I failed to make it work on my machine.</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/ispinfx\"> /u/ispinfx </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51jvai/making_modern_emacs_themes/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51jvai/making_modern_emacs_themes/\">[comments]</a></span>"
    , postGuid = "t3_51jvai"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51jvai/making_modern_emacs_themes/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle = Just "Making modern emacs themes ?"
    }
  , nullPost
    { postAuthor = Just "/u/hicksy994"
    , postDate = Just "2016-09-11T21:26:38.204Z"
    , postDescription =
      Just
        "<!-- SC_OFF --><div class=\"md\"><p>If a brace or parentheses is on the very start of the line, and it becomes highlighted by highlight-parentheses-mode, the line number that is sitting right next to the brace/parentheses becomes bold.</p> <p>It only happens if the brace/parentheses is right at the start of the line. </p> <p>Does anyone know how to stop the line number from becoming bold?</p> </div><!-- SC_ON --> &#32; submitted by &#32; <a href=\"https://www.reddit.com/user/hicksy994\"> /u/hicksy994 </a> <br/> <span><a href=\"https://www.reddit.com/r/emacs/comments/51kuj5/anyone_know_how_to_solve_this_theming_issue_with/\">[link]</a></span> &#32; <span><a href=\"https://www.reddit.com/r/emacs/comments/51kuj5/anyone_know_how_to_solve_this_theming_issue_with/\">[comments]</a></span>"
    , postGuid = "t3_51kuj5"
    , postLink =
      "https://www.reddit.com/r/emacs/comments/51kuj5/anyone_know_how_to_solve_this_theming_issue_with/"
    , postPubdate = Nothing
    , postSummary = Nothing
    , postTitle =
      Just
        "Anyone know how to solve this theming issue with line numbers and highlight-parentheses?"
    }
  ]
