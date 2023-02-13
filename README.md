# Oliver's Config

<!--toc:start-->
- [Oliver's Config](#olivers-config)
  - [WARNING](#warning)
  - [Neovim](#neovim)
    - [Multiple Leaders](#multiple-leaders)
    - [Text objects](#text-objects)
      - [Repeat Mappings](#repeat-mappings)
    - [Some Other Mappings](#some-other-mappings)
      - [Move to start/end of line](#move-to-startend-of-line)
      - [Copy from above](#copy-from-above)
      - [Delete buffer](#delete-buffer)
    - [Command Palette](#command-palette)
      - [Tasks runner](#tasks-runner)
    - [Julia](#julia)
  - [XMonad](#xmonad)
    - [Layouts](#layouts)
      - [Notebook Layout](#notebook-layout)
      - [Layout Bar Avoidance](#layout-bar-avoidance)
      - [Toggleable layouts](#toggleable-layouts)
    - ['Magic' keys](#magic-keys)
    - [Workspaces](#workspaces)
      - [Workspace Harpoon](#workspace-harpoon)
      - [App Harpoon](#app-harpoon)
      - [Browser](#browser)
    - [Other](#other)
      - [Local Modules](#local-modules)
      - [Xmobar Icons](#xmobar-icons)
      - [Rofi script](#rofi-script)
  - [Other Things](#other-things)
    - [Hellslide (Keyboard)](#hellslide-keyboard)
  - [Film picker](#film-picker)
<!--toc:end-->

I'll slowly go through and document some of this stuff. I'm not going to go over
every bit of my configs, but I will cover the interesting (in my opinion) bits.
I'll try and give credit to others where it's due, but there's stuff that I
can't remember where it came from, sorry about that.

## WARNING

This readme could be horrifically out of date with the code in this repo, do not
trust it. It seems I change my configs way more often than I write about them.

Also, my config layout is super inconsistent, and has some utter crap in it, so
be warned.

## Neovim

### Multiple Leaders

The comma leader handles commands that make changes to the buffer in normal use,
but hydras allow for it to be temporarily be changed to git or debug commands
when needed.

Program level commands, like running tests, toggling terminals, opening the
command palette, or using harpoon, are all under the space leader key (the
actually defined leader key).

Square brackets are used as directional leaders (covered more in the Repeat
section), for all manner of jumping about and nothing else (At some point I'll
get rid of all the other things vim defaults to using it for).

I am trying to limit g to just be goto commands, I'll hopefully remove all the
normal extra crap it's used for at some point.

v is a view related key (thanks Kakoune for the inspiration). Unlike Kakoune, I
have v lock into a view mode if any of the bindings you might want to repeat are
pressed. This also done as a hydra.

### Text objects

There are four sets of mappings I'd like to have for each textobject (some
things like quickfix list are listed for completeness, even if only some
commands make sense).

1) Select inside/around the current instance of that text object (mapped to a/i). Obviously this is the basic functionality.
2) Select inside/around the next or last instance of that text object (mapped to al/an/il/in).
3) Jump to the start of the next or last instance of that text object (mapped to [/]). I'd like it so that it always goes to the previous object instead of first going to the start of the current one.
4) Jump to the left or right inside and outside edge of the current instance of that text object (mapped to {/}/(/)).

Scope and block use treesitter-textobjects queries along with mini.ai. Neither
name is great, and block doesn't even match with the mapping (I currently have
it mapped to o). But 'scope' joins @class and @function and block joins @block,
@conditional and @loops.

The current (assuming I remember to update this) state of my mappings cover
these combinations (where one tick is any support and two is preferable
(includes counts and any above conditions)):

| Object                                | Select in/around | Select in/around next/last | Jump to next/last | Jump to left/right |
|:-------------------------------------:|:----------------:|:--------------------------:|:-----------------:|:------------------:|
| scope (method, function, class, etc.) | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| block (loops, conditionals, etc.)     | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| paragraph                             | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| sentence                              | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| sub-word                              | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| word                                  | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| WORD                                  | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| quickfix list item                    | ✗                | ✗                          | ✓                 | ✗                  |
| diagnostic                            | ✗                | ✗                          | ✓                 | ✗                  |
| git signs hunk                        | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| diff hunk                             | ✗                | ✗                          | ✓                 | ✗                  |
| quote                                 | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| bracket                               | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| argument                              | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| function call                         | ✓✓               | ✓✓                         | ✓✓                | ✓✓                 |
| indent                                | ✓                | ✗                          | ✗                 | ✓                  |
| latex section                         | ✓                | ✓✓                         | ✓                 | ✗                  |
| latex environment                     | ✓                | ✓✓                         | ✓                 | ✗                  |
| latex maths                           | ✓                | ✓✓                         | ✓                 | ✗                  |

#### Repeat Mappings

OK, I love this one. I use the jumping around mappings covered above a lot (the
ones bound to [/]), only problem is I hate having to keep pressing the
letters after, makes jumping through a load of spelling mistakes to correct
them all very tedious. So I came up with this little function. Then in all my
mappings for the jumps themselves I add a bit to overwrite the dirJumps variable
like in the example below. I also have all my jump mappings include zz at the
end for that nice screen centring and m\` to add it to the jump list.

vim-slash is used to set dirJumps="search" after each search (and I have it set
to default to that), so that n and N can still be used for searching.

```lua
function _G.commandRepeat(leader, varName)
 local key = vim.api.nvim_get_var(varName)
 if key == "search" then
  if leader == "]" then
   return replace_keycodes("nvv")
  elseif leader == "[" then
   return replace_keycodes("Nvv")
  end
 end
 return replace_keycodes(leader .. key)
end

vim.g.dirJumps = "search"
Map({ "n", "x", "o" }, "n", "v:lua.commandRepeat(']', 'dirJumps')", { expr = true, remap = true })
Map({ "n", "x", "o" }, "N", "v:lua.commandRepeat('[', 'dirJumps')", { expr = true, remap = true })
```

### Some Other Mappings

#### Move to start/end of line

It makes sense to me to have 'big' h and l do a bigger version of the h or
l movement. So this mapping makes H and L go to the start and end of the
line respectively. It's a little more than that though, they go to the first
or last non-whitespace character. If the cursor is already on the first or
last non-whitespace character then it instead goes to the true first or last
character. The exception is for operator pending mode, where L goes to the
proper end of line, I ended up with a lot of trailing whitespace without that.

```lua
Map({ "n", "x", "o" }, "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x" }, "L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], { expr = true })
Map("o", "L", "$")
```

#### Copy from above

This mapping extends the normal c-y mapping to take the whole word from above
the cursor and adds a mapping for below (c-l) the cursor. Credit goes to someone
online for the control y mapping, I've simply extended it to work from below as
well.

This is the last vimscript holdout in my mappings and one of the last at all.
I've had a little go getting it to map in lua, but no success so far.

```vim
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
```

#### Delete buffer

I have a stupidly complicated mapping to decide what I do when I press my magic
key (more on that later) for closing things. It already does a variety of things
depending on what program I'm focused on, but it also does a variety of things
within Neovim depending on what window is focused and what is left on the tab.

### Command Palette

I've tried a few extensions, but none of them did quite what I wanted, so I made
my own. It uses vim.ui.select for the picker (with dressing.nvim to use
telescope for that picker). The main thing was to have a way of adding to the
picker for file types and project types, and having it be able to run vim
commands, lua functions or key maps. I also wanted a way of specifying the
source of a command so that I could group together similar commands.

Honestly it's quite basic, but it works for what I need it for.

#### Tasks runner

OK, moved to overseer and neotest for task and test running. I've got a super
hacky way of attaching a toggleterm terminal to all the overseer tasks (and
therefore neotest tests). Things I still want to do:

- [ ] Overseer tasks that just run lua code.
- [ ] Use the above to add dap debugging to overseer (using the above for the
  starting of tasks, and using dap events to update the task somehow).
- [ ] Get a list of neotest tests in Overseer list, with variations for starting
  them in dap.

### Julia

I am slowly trying collect all the tools I want for Julia development.

nvim-coverage already has some nice Julia support.

I have a made a Julia profile trace reader for perfanno that runs after the
overseer profiling task is finished.

As I can't get dap to work with Julia I have made some functions to toggle
Debugger.jl and Infiltrator.jl breakpoints, including adding the using statement
if the file doesn't have one and removing it when the last breakpoint is
removed.

So still on the list to do at some point is to (not that I'll ever get around to
them):

- [x] Add Julia support for neogen (someone else has already started on that).
  Covered by the LSP adding doc string updating and luasnip being as good as it is.
- [x] Make a Julia server to run tests and finish the neotest adaptor.
- [x] Make a similar thing for benchmarks, and probably a neotest consumer for
  benchmark results.
- [x] Make a better profile data exporter.
- [x] Extend the profile runner and exporter to allocation profiles.
- [x] Something like splitjoin.vim. Covered by ts-node-actions and JuliaFormatter
- [ ] Figure out if a wrapper can be made to use DebugAdapter.jl in Neovim.

## XMonad

### Layouts

#### Notebook Layout

OK, so this has a bit of a weird name, it came from the original use case and
I just can't be bothered to think of a better one. So the original use case, I
wanted to have source code in the middle, a browser on the left and an output
window on the right (often a Jupyter notebook, hence the name, or a PDF reader).
These main three programs need to stay as tall as possible. Any other program
that I opened after this wasn't as important, so it's fine to just be squashed
to the bottom. After a couple of rewrites and much feature creep, this layout
now handles 90% of my uses (9.99% are dealt with the toggleable layouts, I
basically don't use the tab one).

So the basic explanation is that there are three window types, main, column and
stack. The main and column windows share the horizontal space, with a weighting
towards the main windows (I default to having them 2x the width). When more
windows are opened, the start to fill the stack. The stack pushes up the column
and main windows one by one as the stack runs out of space, using the assuming
that the width of a stack window can't be less than the width of a column
window. Once the stack has filled up the width of the screen, it just keeps
splitting that one width between any new windows (but that's like 10+ windows,
only time it ever comes up is running D&D sessions with each NPC character sheet
in its own window).

The main and column windows have a bit of customization on how they share the
top. They can either start at the edge and go across or start in the middle and
stagger out. On top of this the direction can be flipped, so they can start from
the right side or from the middle but with a right bias. The number of main and
column windows and the ratio of their sizes can be set. The direction that the
stack comes in (and swallows up precious space) from can be flipped as well,
along with the percentage height of the screen that they take up. And of course
all of these can be done with key-bindings.

The last bit of feature creep is still in the works, dynamic layouts based on
resolution. This can be done in XMonad normally, but by keeping it in the layout
it means it can keep using the same variables for number of different types of
windows, and directions. The idea behind this is that the main windows can all
be tabbed on top of each other, and so on for the column and tabbed windows.
My motivation to work on this right now is low as it is only used on my second
monitor, but if I ever move to a laptop then it will be killer. I'll have to get
my head around the logic of the persistent two pane layouts and try and nick
that.

OK, I gave up on the above paragraph for now. At the moment I just have it swap
to a modified version of the tall layout for 1080p screens.

I love this layout so much that not only is it basically all I use, but I also
use it twice. In that I have many layouts defined that use the notebook layout
just with different default settings for different workspaces.

#### Layout Bar Avoidance

Inspired by a Reddit post, I have made a little function that does some
hardcoded checks for the location of window columns, and extends them if they
don't interfere with the Xmobar. I've added this to my Notebook and Four column
layout. Basically, I have the xmobar take up the centre third of the top of the
screen, on those layouts there are checks to see if a column is in the centre
third of the screen horizontally. If not, it will extend the column height by
the height of my Xmobar, it's only a little of screen space that I gain back,
honestly it is mostly for the looks.

This is done with some strategic uses of the following function:

```haskell
modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx _ _ _)=
    Rectangle sx y sw h
    where   ymoddifier= if toInteger (8 + sx) < toInteger bx + 960
                        then 31
                        else 0
            y = sy + ymoddifier
            h = sh - fromIntegral ymoddifier
```

#### Toggleable layouts

The only non notebook layouts I actually use are the three toggleable layouts I have set up. The
first one is toggleable full-screen. A fairly standard one that lets me have the best of both worlds
for full-screening, allowing me to have a contained maximize within a window and then choose to make
it actually full screen.

Fullbar takes all the windows on a workspace and tabs them, so that it's kind of full screen but
keeping the status bar and also having a way of seeing what's open. Centre focus is the same, but
with big gaps on the sides. This is great for big monitors, being able to keep the thing you're
looking at right in front of you instead of way off to one side. I have this set up per workspace,
so normally it defaults to half the screen width, but on my thesis workspace it instead uses roughly
the width of 100 character columns in my terminal. For that extra focused writing experience.

### 'Magic' keys

Everything in this section will be using at least one 'server mode' of a
program, mostly neovim-remote (nvr), kitty remote and XMonad server mode.

There are so many things that need shortcuts, and so few keys, especially on my
tiny keyboard. I try and keep OS level shortcuts all only on the Super key. But
it can be a bit restrictive sometimes. It's also super annoying when different
programs have different shortcuts to do the same thing (why doesn't every
program let me customize shortcuts???). These key bindings try and help with
that by changing based on context. Things like super+right and super+left always
swapping tabs backward and forward, in programs or in layouts with tabs.

It also helps to have OS level shortcuts work as program level shortcuts
depending on what is focused. The best example of this is window move commands
(selecting the window to the left, right, top or bottom of the current window),
where if the editor is focused then the shortcut should move between editor
splits, the same with terminal splits, and if something without splits is
used it should just move between windows. The killer is having it move out of
the editor or terminal if it is already at the edge. That is done by using a
modified version of vim-kitty-navigator and a bash script (along with kitty
remote) for the editor and terminal respectively. This allows all windows and
splits to be treated as one continuous thing as far as 2d navigation is concerned.

Other 'Magic' keys include:

- Super+backspace closes Neovim windows (or buffers if it's on the last window),
terminal windows, or window manager windows depending on what is focused.
- Super+m moves the window to the master pane, unless the terminal is focused, in
which case it moves the terminal window to the terminal master. If the terminal
window is already in the terminal's master window then it will move to the
window manager master (this is super hacky, relying on the terminal master and
stack windows having different dimensions and also relying on a sleep in the
script).
- Super+w sends the full-screen key (normally f11). Unless it's focused on a
browser with the current tab being a YouTube tab, in which case it sends the
f key, to full-screen the YouTube video. If a terminal is focused then it
full-screens the current window within the terminal, but if Neovim is focused
and there are no other terminal windows in the tab then it will focus the
current Neovim window (using mini.misc zoom).
- Super+left/right changes tabs in the browser, tabs in kitty or tabs in Neovim
  (or terminals in toggleterm).

### Workspaces

There's a bit of inconsistency in my workspaces because I use workspaces in two
different ways. For specific programs or specific projects. Workspaces like
print are program specific, in that case it's my slicer. And workspaces like sim
are project specific, in that case for the simulation I'm programming for my
PhD.

#### Workspace Harpoon

I really like the harpoon Neovim extension for quickly jumping between files in
Neovim. Well my attitude to XMonad workspaces (or projects or whatever) is to
have a tonne of them, way more than I have space for dedicated shortcuts. So the
way I solved this was to make a little bash script that can jump between a small
list of workspaces, and a way of editing that list. Neovim harpoon lets you have
a list of files per directory, to get a similar thing for my workspace harpoon I
have a concept of preset lists of workspaces, so I can quickly swap between the
set I use for my work simulations, thesis writing or config editing, etc.

#### App Harpoon

I also added a workspace dependent app harpoon for the home row on my right
hand. Where each key runs or raises the set app (with the pinky finger always
having the workspace specific browser).

#### Browser

Each workspace (and I have a lot of them) has its own user-data-dir (using
altercation's browser script) so that tabs can be picked up from where I left
off last time I was in that workspace.

### Other

#### Local Modules

ConditionalKeys is from Ethan (where my XMonad config originated from).
CycleWSLocal strips out everything I don't use and adds a function to send the
current window to the previous workspace, as well as using all none focused
workspaces as possible ones to switch to (instead of just the non-visible), so
it also works for moving workspaces between monitors. WindowGoLocal just changes
the runOrRaise function to only look on the current workspace. NamedScratchpad
has been modified to add a function that sends any scratchpad back to the NSP
workspace, so I can use a rofi script to summon scratchpads, but still send them
away quickly.

#### Xmobar Icons

I wanted to use nerdfont icons to represent things like battery level and
network connection. I don't think there is a good way to do this in xmobar, so I
made a little bash script that would output these icons and then had xmobar run
that script. It's nothing fancy, just checks the battery state and returns the
relevant icon, and does the same with network state. It also adds some actions
to the icons, so that clicking on them makes a notification with some more info
in it. It'd be nice to have that information in hover text, but it doesn't look
like that's possible with xmobar, maybe it's time to move to another bar. It
also adds a bluetooth icon, at the moment it doesn't track bluetooth state, just
provides a shortcut to bluetooth settings.

#### Rofi script

I used to have a load of keyboard shortcuts for things like layout settings, or
media controls. So instead of just committing, I made a rofi script for calling
them. This also allowed me to add a menu for the display control stuff,
bluetooth, network and sound control stuff (most of which was a mouse task
before), making it all keyboard accessible now.

## Other Things

### Hellslide (Keyboard)

A simple wireless low profile keyboard that is based on the kyria's shape, but
with the two outermost thumb keys moved to below the middle and ring finger
columns. It also gets rid of the outer column, almost all none key features and
uses Kailh choc low profile switch spacing.

One of my goals when it came to making the mapping for this board was to avoid
layers. I've done this using combos for all the numbers and symbols and having
all the other keys (like space, tab, del etc.) on the thumb clusters, with combo
thumb clusters for mod taps. I only have one layer for function keys, consol
swapping (control + alt + function keys) and bluetooth and input control.

And of course because it's a wireless split keyboard, I had to make it so the two
halves magnet together side by side to make a more stable keyboard for used on
my lap. And magnet together back to back for easy transport.

At the moment I am using a slightly crappy FDM PLA case for it, but I have got a
laser sintered nylon case that I managed to sneak onto a build at work that I
just need to get around to dying before I change to it.

## Film picker

Have you ever had 4 hours and 20 minutes to spare and wondered what combination
of films you could watch to perfectly fill your time? Well now with the handy
film picker script you can not only find out, but also queue them up.

If you run this script in a folder filled with videos it will open up a Neovim
instance with a list of all the films sorted by duration. The winbar defined in
ui_config.lua will display the duration of all the films in the current
paragraph. When the Neovim instance is quit whatever paragraph is at the top of
the file will be queued up in MPV. (The lsv script is also required).
