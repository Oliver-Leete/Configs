<!-- markdownlint-disable MD013 -->
# Oliver's Config

I'll slowly go through and document some of this stuff I'm not going to go over
every bit of my configs, but it might be worth some of the things I'm doing that
I haven't seen elsewhere (or not as much as I think I should be). I'll try and
give credit to others where it's due, but there's stuff that I can't remember
where it came from.

## WARNING

This readme is horrifically out of date with the code in this repo, do not trust
it. It seems I change my configs way more often than I write about them.

# Neovim

## Multiple Leaders

I am slowly moving things over to a system of having a few top level leaders,
each with its own use. Space, the actual leader key set, is used for all
'program level commands', things like fuzzy finding, running code, debugging,
etc. Comma, the user command key, is used for additional editing commands, like
aligning things, changing case, and special pasting. Backslash, the local leader
key, is used for very language specific things. Such as viewing a Tex file's
PDF, or using the VimTex word count function.

Square brackets are used as directional leaders (covered more in the Repeat
section), for all manner of jumping about. I am trying to limit g to just be
goto commands (goto paste works kinda), I'll hopefully remove all the normal
extra crap it's used for at some point. v is a view related key (thanks Kakoune
for the inspiration). I have a bit of a hacky way to lock a mode in, by simply
having the mapping end by calling the same leader key again, this has many
flaws, but it is the best I can be bothered to do at the moment.

## Repeat Mappings

OK, I love this one. I use a lot of jumping around mappings, bound to ] and [
for forward and backward jumps respectively. Especially treesitter text objects
movements. I have my shift keys bound to send the brackets on a press (but
still send the shift key when held) in my keyboard firmware. Only problem is I
hate having to keep pressing the letters after, makes jumping through a load
of spelling mistakes to correct them all very tedious. So I came up with this
little function. Then in all my mappings for the jumps themselves I add a bit
to overwrite the dirJumps variable like in the example below (note that it's in
which-key.nvim format). I also have all my jump mappings include zz at the end
for that nice screen centering and m\` to add it to the jump list (I think it
should be m', but that doesn't seem to work)

```lua
vim.api.nvim_set_var("dirJumps", "f")

function _G.commandRepeat(leader, varName)
    local jump = vim.api.nvim_get_var(varName)
    return vim.api.nvim_replace_termcodes(leader .. jump, true, true, true)
end

    n = { "v:lua.commandRepeat(']', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
    N = { "v:lua.commandRepeat('[', 'dirJumps')", "Repeat Last", expr = true, noremap = false },

    ["]"] = {
        f = { "<cmd>let g:dirJumps='f'<cr><cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Function" },
    }
```

Since writing this, I have moved the repeat key to n and N, with jumping between
search results being the default for all file types. ]], ][, [[ and [] now jump
to the start or end of the previous file type based defaults (mostly scopes, but
sections in Tex or markdown files).

Realizing that I could use the same function for all kinds of repeats. I've
already added it to my panel opening mappings (all bound under \<leader\>v) so
that \<leader\>vv will open the last panel, or close it if it's still open. This
is just done by having another variable to store the window command in.

All of these also have a default command that is used if nothing has been called
yet (can't repeat what hasn't been done). The default panel is nvim tree, but
the below snippet can be used to set it per file type, like setting it to open
the table of contents in a Tex file.

```lua
if vim.api.nvim_get_var("panelRepeat") == "x" then
    vim.api.nvim_set_var("panelRepeat", "c")
end
```

## Target Mappings

OK, so I'm a massive fan of the next/last operators provided by the targets
extension. Only problem is that they don't work with every text object. I've
started to make some bindings for that will fill in the gaps, so far I've got
this to add target mappings for Treesitter text objects:

```lua
function _G.ts_target(count, object)
    vim.cmd("TSTextobjectGotoNextStart " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoNextStart " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectSelect " .. object)
end

function _G.ts_target_back(count, object)
    vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectGotoPreviousStart " .. object)
    vim.cmd("TSTextobjectSelect " .. object)
end
```

These use the functions provided by the TS Text Object plugin to define two
new commands. They move forward or backward a count number of the desired
text object and then select said object. This can be used in addition to a
c-u mapping to make an operator pending mapping (examples can be seen in my
operator pending mappings). I have also made a similar function for git hunks,
using the git signs plugin. This is slightly simpler, due to the way the git
signs function works, only one function is needed, with one command swapped out
depending on direction.

Just made some more functions and mappings for this. Mapped targets allows
for the creation of target mappings using already existing mapped motions and
selections. Plug targets instead uses the plug mapping provided by plugins.
Examples of all of these functions can be seen in my o_bindings_config.Lua and
Tex.Lua config files. Combined with the targets plugin this allows for almost
any text object to be targeted from afar. The one remaining object has been
conquered, paragraphs. This had to have two functions all to itself, but it now
works how I want it to.

## Targeted Paste

This is something I've wanted for a while, a simple little pair of functions
that lets you paste from afar. This is useful for putting the thing you just
yanked at the end of the paragraph (or any other text object you could think
of).

The first is the setup function, followed by the function that can be set as the
Opfunc (for whatever reason the Opfunc can't take arguments, hence the setup
function).

```lua
function _G.pre_paste_away(register, paste)
    local direction
    if string.find(paste, "p") then
        direction = "]"
    else
        direction = "["
    end
    Paste_away_direction = direction
    Paste_away_register = register
    Paste_away_paste = paste
end

function _G.paste_away()
    vim.cmd([[normal ']] .. Paste_away_direction .. '"' .. Paste_away_register .. Paste_away_paste)
end
```

Turns out I don't ever actually use this, I'm not even sure if I even still have
it bound to anything. Although I have considered making everything targeted by
default, like insert mode, ext. Maybe that would make me use it.

## Diff View Mappings

I have a few commands to let me pick what to diff against in diff view, so I
added a variation of the repeat mappings to give me a mapping of reopening diff
view using the last command. The telescope commands come from someone from the
Neovim subreddit, the only extra bit is the bit that stores the command (and in
the actual config there are more of them, for picking things like branches).

```lua
vim.api.nvim_set_var("DiffviewLast", "DiffviewOpen")

function _G.diff_repeat()
    local cmd = vim.api.nvim_get_var("DiffviewLast")
    vim.cmd(cmd)
end
local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
function _G.git_commits_againsthead()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
```

## Some Other Mappings

These two make j and k respect wrapped lines, unless a count is given, in which
case j and k act on true lines. This means that normally they make the cursor
go where you'd expect (if you've ever used any other editor), but relative line
numbers can still be used for fast jumping. In addition, they also add to the
jump list if a count greater than 5 is given.

```vim
nnoremap <expr> j v:count?(v:count>5?"m'".v:count:'').'j':'gj'
nnoremap <expr> k v:count?(v:count>5?"m'".v:count:'').'k':'gk'
```

It makes sense to me to have 'big' h and l do a bigger version of the h or
l movement. So this mapping makes H and L go to the start and end of the
line respectively. It's a little more than that though, they go to the first
or last non-whitespace character. If the cursor is already on the first or
last non-whitespace character then it instead goes to the true first or last
character.

```vim
nnoremap <expr> H getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^'
nnoremap <expr> L getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_'
```

## The Slow Road To Kak

I really like the ideas behind Kakoune, and will at some point have another try
at moving to it (or Helix). But for the mean time I'm just taking a few ideas
from it.

### Kak Mappings

I don't actually know how I would get to normal visual mode at the moment, I
have unmapped all the v key functionality and instead split line and column
based stuff to x and C, inspired by the Kak mappings. These mappings also allow
for quickly expanding the selections (Why would you go into column mode if you
aren't going to select another column?)

```vim
nnoremap x V
nnoremap X V
nnoremap C <c-v>j
nnoremap <m-C> <c-v>k


xnoremap x j$
xnoremap X <esc>`<kV`>
xnoremap C j
xnoremap <m-C> <esc>`<k<c-v>`>
xnoremap <M-x> v
xnoremap <M-;> o
```

### Remote Terminal Tasks

I tried for ages to get a system for running terminal tasks set up in NVim that
I was happy with, closest I got was using toggle term, but I still had some
issues. When I looked back into Kakoune I thought about the idea of keeping the
editor for editing only, and started looking into running terminal tasks in my
actual terminal. Using Kitty's remote functionality I was able to set up some
shortcuts for running REPLs, debug and build tools.

There are a few scripts used for this. Oneshot is for things like build tasks,
where the terminal is killed and recreated every time the task is called.
Persistent is used for things like debug terminals or test runners, where you
want the terminal to stick around whist more commands are sent to it. And REPL
is used for its ability to send strings from the editor windows.

Another script can grab the output from the latest command in a terminal and
bring it in to the quickfix list. Or the other way around, some shortcuts in
kitty can be used to jump to the location of an error message (this would work
better with another editor, if they don't have a quickfix list alternative).

## New Text Object Concept

I haven't put much time into implementing this yet, but I have plans. The plan
is to have all text objects accessable by alphabetic characters (so the capital
letters can be used for some stuff) or characters that can be shifted. And then
to use a few directional keys to select them in different ways. The first set
would be used ([ and ] in my current plans) to jump to the next or previous
object or in operator pending, to act until the next object. The next pair ({
and } in my current plans) would select within the next text object, similar to
gn and gN. And the last pair (( and ) in my current plans), would jump to the
start or end of the current text object, like vim ninja feet.

Having this would allow for things like yanking the next two functions, deleting
to the next bracket and then changing to the end of the quote the cursor is
currently in.

I have started working on this now. I'm trying to think about how to make it
customisable, both for easy changes for my own config, but also to be able
to make it into an extension. At the moment it uses custom methods for some
of the built in text objects, like words, paragraphs ect., targets is used
for brackets, arguments and quotes, vim-word-motion is used for subwords and
treesitter text objects is used for functions and that stuff. I have altered the
queries in TSTextobjects to simplyfy them a bit, using just three instead of the
many, parameters is the same as it is. Scope takes over for function and objects
and block takes over for conditionals, loops and blocks.

### Other editors

For when I actually want the full powers of Kakoune I have a shortcut that uses
kitty's remote control to open a new tab with Kak running. This tab is opened
to the same cursor position in the same file as I ran the shortcut from. I also
have shortcuts for opening in vis and helix, but I don't tend to use any of
these.

## Insert Mappings

This little function handles toggling of the completion pop-up. I didn't want
two separate mappings for opening and closing, so this makes control space a
toggle instead. This uses a bit of a hacky approach of first making the compe
close command a plug mapping, but I couldn't get it to work any other way.

```lua
_G.compe_toggle = function()
    if vim.fn.pumvisible() == 1 then
        -- return replace_keycodes("<esc>:call compe#close()<cr>a")
        return replace_keycodes("<plug>(compe-close)")
    else
        return replace_keycodes("<cmd>call compe#complete()<cr>")
    end
end

vim.api.nvim_set_keymap("i", "<c-space>", "v:lua.compe_toggle()", {expr = true})
vim.api.nvim_set_keymap("s", "<c-space>", "v:lua.compe_toggle()", {expr = true})
```

```vim
inoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')
snoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')
```

I just use the standard CMP and autopairs mappings. Ctrl+] is used for next
snippet selection, and expanding is just done with the completion menu.

#### Copy from above

This mapping takes the word from above (c-y) or below (c-l) the cursor. Credit
goes to someone online for the control y mapping, I've simply extended it to
work from below as well.

```vim
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
```

# XMonad

## Layouts

### Notebook Layout

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

I love this layout so much that not only is it basically all I use, but I also
use it twice. In that I have two different layouts defined that use the notebook
layout just with different default settings (even more than twice now that I'm
using multiple screen resolutions).

### Layout Bar Avoidance

Inspired by a Reddit post, I have made a little function that does some
hardcoded checks for the location of window columns, and extends them if they
don't interfere with the XMobar. I've added this to my Notebook and Four column
layout. Basically, I have the xmobar take up the centre third of the top of the
screen, on those layouts there are checks to see if a column is in the centre
third of the screen horizontally. If not, it will extend the column height by
the height of my XMobar, it's only a little of screen space that I gain back,
honestly it is mostly for the looks.

This is done with some strategic uses of the following function:

```haskell
modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx _ _ _)=
    Rectangle sx y sw h
    where   ymoddifier= if toInteger (8 + sx) < toInteger bx + 1280
                        then 31
                        else 0
            y = sy + ymoddifier
            h = sh - fromIntegral ymoddifier
```

### Toggleable layouts

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

## 'Magic' keys

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
splits to be treated as one continuos thing as far as 2d navigation is concerned.

Other 'Magic' keys include:

- Super+backspace closes Neovim windows (or buffers if it's on the last window),
terminal windows, or window manager windows depending on what is focused.
- Super+m moves the window to the master pane, unless the terminal is focused, in
which case it moves the terminal window to the terminal master. If the terminal
window is already in the terminal's master window then it will move to the
window manager master (this is super hacky, relying on the terminal master and
stack windows having different dimensions and also relying on a sleep in the
script).
- Super+n focuses the main editor for the workspace, or creates a new terminal
split if the editor is focused.

## Workspace Harpoon

I really like the harpoon Neovim extension for quickly jumping between files in
Neovim. Well my attitude to XMonad workspaces (or projects or whatever) is to
have a tonne of them, way more than I have space for dedicated shortcuts. So the
way I solved this was to make a little bash script that can jump between a small
list of workspaces, and a way of editing that list. Neovim harpoon lets you have
a list of files per directory, to get a similar thing for my workspace harpoon I
have a concept of preset lists of workspaces, so I can quickly swap between the
set I use for my work simulations, thesis writing or config editing.

## Other

### Local Modules

ConditionalKeys is from Ethan (where my XMonad config originated from).
CycleWSLocal strips out everything I don't use and adds a function to send the
current window to the previous workspace, as well as using all none focused
workspaces as possible ones to switch to (instead of just the non-visible), so
it also works for moving workspaces between monitors. WindowGoLocal just changes
the runOrRaise function to only look on the current workspace. NamedScratchpad
has been modified to add a function that sends any scratchpad back to the NSP
workspace, so I can use a rofi script to summon scratchpads, but still send them
away quickly.

### Xmobar Icons

I wanted to use nerdfont icons to represent things like battery level and
network connection. I don't think there is a good way to do this in xmobar, so I
made a litte bash script that would output these icons and then had xmobar run
that script. It's nothing fancy, just checks the battery state and returns the
relevant icon, and does the same with network state. It also adds some actions
to the icons, so that clicking on them makes a notification with some more info
in it. It'd be nice to have that information in hover text, but it doesn't look
like that's possible with xmobar, maybe it's time to move to another bar. It
also adds a bluetooth icon, at the moment it doesn't track bluetooth state, just
provides a shortcut to bluetooth settings.

### Rofi script

I used to have a load of keyboard shortcuts for things like layout settings, or
media controls. So instead of just committing, I made a rofi script for calling
them. This also allowed me to add a menu for the display control stuff,
bluetooth, network and sound control stuff (most of which was a mouse task
before), making it all keyboard accessible now.

# Hellslide (Keyboard)

A simple wireless low profile keyboard that is based on the kyria's shape, but
with the two outermost thumb keys moved to below the middle and ring finger
columns. It also gets rid of the outer column, almost all none key features and
uses Kailh choc low profile switch spacing.

One of my goals when it came to making the mapping for this board was to avoid
layers. I've done this using combos for all of the numbers and symbols and
having all the other keys (like space, tab, del etc.) on the thumb clusters,
with combo thumb clusters for mod taps. I only have one layer for function keys,
consol swapping (control + alt + function keys) and bluetooth and input control.
