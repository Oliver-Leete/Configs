<!-- markdownlint-disable MD013 -->
# Oliver's Config

I'll slowly go through and document some of this stuff I'm not going to go over every bit of my
configs, but it might be worth some of the things I'm doing that I haven't seen elsewhere (or not as
much as I think I should be). I'll try and give credit to others where it's due, but there's stuff
that I can't remember where it came from.

# Neovim

## Multiple Leaders

I am slowly moving things over to a system of having a few top level leaders, each with its own
use. Space, the actual leader key set, is used for all 'program level commands', things like fuzzy
finding, running code, debugging, etc. Comma, the user command key, is used for additional editing
commands, like aligning things, changing case, and special pasting. Backslash, the local leader key,
is used for very language specific things. Such as viewing a Tex file's PDF, or using the VimTex
word count function.

Square brackets are used as directional leaders (covered more in the Repeat section), for all manner
of jumping about. I am trying to limit g to just be goto commands (goto paste works kinda), I'll
hopefully remove all the normal extra crap it's used for at some point. v is a view related key
(thanks Kakoune for the inspiration). I have a bit of a hacky way to lock a mode in, by simply
having the mapping end by calling the same leader key again, this has many flaws, but it is the best
I can be bothered to do at the moment.

## Repeat Mappings

OK, I love this one. I use a lot of jumping around mappings, bound to ] and [ for forward and
backward jumps respectively. Especially treesitter text objects movements. I have my shift keys
bound to send the brackets on a press (but still send the shift key when held) in my keyboard
firmware. Only problem is I hate having to keep pressing the letters after, makes jumping through a
load of spelling mistakes to correct them all very tedious. So I came up with this little function.
Then in all my mappings for the jumps themselves I add a bit to overwrite the dirJumps variable like
in the example below (note that it's in which-key.nvim format). I also have all my jump mappings
include zz at the end for that nice screen centering and m\` to add it to the jump list (I think
it should be m', but that doesn't seem to work)

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

Since writing this, I have moved the repeat key to n and N, with jumping between search results being
the default for all file types. ]], ][, [[ and [] now jump to the start or end of the previous
file type based defaults (mostly scopes, but sections in Tex or markdown files).

Realizing that I could use the same function for all kinds of repeats. I've already added it to my
panel opening mappings (all bound under \<leader\>v) so that \<leader\>vv will open the last panel,
or close it if it's still open. This is just done by having another variable to store the window
command in.

All of these also have a default command that is used if nothing has been called yet (can't repeat
what hasn't been done). The default panel is nvim tree, but the below snippet can be used to set it
per file type, like setting it to open the table of contents in a Tex file.

```lua
if vim.api.nvim_get_var("panelRepeat") == "x" then
    vim.api.nvim_set_var("panelRepeat", "c")
end
```

## Target Mappings

OK, so I'm a massive fan of the next/last operators provided by the targets extension. Only problem
is that they don't work with every text object. I've started to make some bindings for that will
fill in the gaps, so far I've got this to add target mappings for Treesitter text objects:

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

These use the functions provided by the TS Text Object plugin to define two new commands. They move
forward or backward a count number of the desired text object and then select said object. This can
be used in addition to a c-u mapping to make an operator pending mapping (examples can be seen in my
operator pending mappings). I have also made a similar function for git hunks, using the git signs
plugin. This is slightly simpler, due to the way the git signs function works, only one function is
needed, with one command swapped out depending on direction.

Just made some more functions and mappings for this. Mapped targets allows for the creation
of target mappings using already existing mapped motions and selections. Plug targets instead
uses the plug mapping provided by plugins. Examples of all of these functions can be seen in my
o_bindings_config.Lua and Tex.Lua config files. Combined with the targets plugin this allows for
almost any text object to be targeted from afar. The one remaining object has been conquered,
paragraphs. This had to have two functions all to itself, but it now works how I want it to.

## Targeted Paste

This is something I've wanted for a while, a simple little pair of functions that lets you paste
from afar. This is useful for putting the thing you just yanked at the end of the paragraph (or any
other text object you could think of).

The first is the setup function, followed by the function that can be set as the Opfunc (for
whatever reason the Opfunc can't take arguments, hence the setup function).

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

## Diff View Mappings

I have a few commands to let me pick what to diff against in diff view, so I added a variation of
the above to give me a mapping of reopening diff view using the last command. The telescope commands
come from someone from the Neovim subreddit, the only extra bit is the bit that stores the command
(and in the actual config there are more of them, for picking things like branches).

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

These two make j and k respect wrapped lines, unless a count is given, in which case j and k act on
true lines. This means that normally they make the cursor go where you'd expect (if you've ever used
any other editor), but relative line numbers can still be used for fast jumping. In addition, they
also add to the jump list if a count greater than 5 is given.

```vim
nnoremap <expr> j v:count?(v:count>5?"m'".v:count:'').'j':'gj'
nnoremap <expr> k v:count?(v:count>5?"m'".v:count:'').'k':'gk'
```

It makes sense to me to have 'big' h and l do a bigger version of the h or l movement. So this
mapping makes H and L go to the start and end of the line respectively. It's a little more than that
though, they go to the first or last non-whitespace character. If the cursor is already on the first
or last non-whitespace character then it instead goes to the true first or last character.

```vim
nnoremap <expr> H getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^'
nnoremap <expr> L getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_'
```

## The Slow Road To Kak

I really like the ideas behind Kakoune, and will at some point have another try at moving to it. But
for the mean time I'm just taking a few ideas from it.

### Kak Mappings

I don't actually know how I would get to normal visual mode at the moment, I have unmapped all the
v key functionality and instead split line and column based stuff to x and C, inspired by the Kak
mappings. These mappings also allow for quickly expanding the selections (Why would you go into
column mode if you aren't going to select another column?)

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

I tried for ages to get a system for running terminal tasks set up in NVim that I was happy with,
closest I got was using toggle term, but I still had some issues. When I looked back into Kakoune
I thought about the idea of keeping the editor for editing only, and started looking into running
terminal tasks in my actual terminal. Using Kitty's remote functionality I was able to set up some
shortcuts for running REPLs, debug and build tools.

Three scripts are used for this. The first is for one shot tasks, it opens up a terminal window in
the same tab as the editor, runs the given command and then runs a command to send the output to the
quickfix list (using the current errorformat).

## New Text Object Concept

I haven't put much time into implementing this yet, but I have plans. The plan is to have all text
objects accessable by alphabetic characters (so the capital letters can be used for some stuff). And
then to use a few directional keys to select them in different ways. The first set would be used ([
and ] in my current plans) to jump to the next or previous object or in operator pending, to act
until the next object. The next pair ({ and } in my current plans) would select within the next text
object, similar to gn and gN. And the last pair (( and ) in my current plans), would jump to the
start or end of the current text object, like vim ninja feet.

Having this would allow for things like yanking the next two functions, deleting to the next bracket
and then changing to the end of the quote the cursor is currently in.

<!-- TODO: describe other scripts -->

For when I actually want the full powers of Kakoune I have a shortcut that uses kitty's remote
control to open a new tab with Kak running. This tab is opened to the same cursor position in the
same file as I ran the shortcut from.

## Insert Mappings

### Snippets and Completion

With a bunch of completion stuff comes a lot of mapping overlaps This is how I've tried to fix them.

First up is the usual multi-purpose tab mappings. Nothing too special here, I don't use tab for
scrolling completions (my arrow keys are just too easy to get to) but I do use it for tabbing out of
brackets.

```lua
local luasnip = require("luasnip")
_G.tab_complete = function()
    if luasnip and luasnip.expand_or_jumpable() then
        return replace_keycodes("<Plug>luasnip-expand-or-jump")
    else
        return replace_keycodes("<tab>")
    end
end

_G.s_tab_complete = function()
    if luasnip and luasnip.jumpable(-1) then
        return replace_keycodes("<Plug>luasnip-jump-prev")
    else
        return replace_keycodes("<s-tab>")
    end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
```

Next up is toggling of the completion pop-up. I didn't want two separate mappings for opening and
closing, so this makes control space a toggle instead. This uses a bit of a hacky approach of first
making the compe close command a plug mapping, but I couldn't get it to work any other way.

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

This next bit is depreciated since moving to compe. I just use the standard CMP and autopairs
mappings. Ctrl+] is used for next snippet selection, and expanding is just done with the completion
menu.

Enter is possibly the most used key, used for line breaks, nvim-autopairs splitting, completion
confirmation and changing the choices in luasnip. I've let autopairs deal with most of it, just
adding in a check for luasnip changeability (and now an extra check for luasnip expandability). I
think this might cause some issues in some snippets, but none that I've used so far. Furthermore,
I'm currently trying out this without the snippet choice part, to try and limit the amount of
accidental snippet overwriting I do.

```lua
_G.enter_complete = function()
    if vim.fn.pumvisible() == 1 then
        if luasnip and luasnip.expandable() then
            return replace_keycodes("<plug>luasnip-expand-snippet")
        else
            return replace_keycodes("<cr>")
        end
    elseif luasnip and luasnip.choice_active() then
        return replace_keycodes("<plug>luasnip-next-choice")
    else
        return require('nvim-autopairs').autopairs_cr()
    end
end

vim.api.nvim_set_keymap("i", "<cr>", "v:lua.enter_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<cr>", "v:lua.enter_complete()", { expr = true })
```

#### Copy from above

This mapping takes the word from above (c-y) or below (c-l) the cursor. Credit goes to someone
online for the control y mapping, I've simply extended it to work from below as well.

```vim
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
```

# XMonad

## Layouts

### Notebook Layout

OK, so this has a bit of a weird name, it came from the original use case and I just can't be
bothered to think of a better one. So the original use case, I wanted to have source code in the
middle, a browser on the left and an output window on the right (often a Jupyter notebook, hence
the name, or a PDF reader). These main three programs need to stay as tall as possible. Any other
program that I opened after this wasn't as important, so it's fine to just be squashed to the
bottom. After a couple of rewrites and much feature creep, this layout now handles 90% of my uses
(9.99% are dealt with the toggleable layouts, I basically don't use the tab one).

So the basic explanation is that there are three window types, main, column and stack. The main and
column windows share the horizontal space, with a weighting towards the main windows (I default to
having them 2x the width). When more windows are opened, the start to fill the stack. The stack
pushes up the column and main windows one by one as the stack runs out of space, using the assuming
that the width of a stack window can't be less than the width of a column window. Once the stack has
filled up the width of the screen, it just keeps splitting that one width between any new windows
(but that's like 10+ windows, only time it ever comes up is running D&D sessions with each NPC
character sheet in its own window).

The main and column windows have a bit of customization on how they share the top. They can either
start at the edge and go across or start in the middle and stagger out. On top of this the direction
can be flipped, so they can start from the right side or from the middle but with a right bias.
The number of main and column windows and the ratio of their sizes can be set. The direction that
the stack comes in (and swallows up precious space) from can be flipped as well, along with the
percentage height of the screen that they take up. And of course all of these can be done with
key-bindings.

The last bit of feature creep is still in the works, dynamic layouts based on resolution. This can
be done in XMonad normally, but by keeping it in the layout it means it can keep using the same
variables for number of different types of windows, and directions. The idea behind this is that the
main windows can all be tabbed on top of each other, and so on for the column and tabbed windows. My
motivation to work on this right now is low as it is only used on my second monitor, but if I ever
move to a laptop then it will be killer. I'll have to get my head around the logic of the persistent
two pane layouts and try and nick that.

I've also got it set up with sub-layout, so I can have tabs on the main layout for those D&D games
(keep character sheets that won't appear at the same time in a tab group to save a bit of space). It
also works nicely when changing to a lower resolution screen (see above).

I love this layout so much that not only is it basically all I use, but I also use it twice. In
that I have two different layouts defined that use the notebook layout just with different default
settings.

### Layout Bar Avoidance

Inspired by a Reddit post, I have made a little function that does some hardcoded checks for the
location of window columns, and extends them if they don't interfere with the XMobar. I've added
this to my Notebook and Four column layout. Basically, I have the xmobar take up the centre third of
the top of the screen, on those layouts there are checks to see if a column is in the centre third
of the screen horizontally. If not, it will extend the column height by the height of my XMobar, it's
only a little of screen space that I gain back, but it's still nice.

This is done with some strategic uses of the following function:

```haskell
modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx _ bw _)=
    Rectangle sx y sw h
    where   ymoddifier= if (toInteger (fromIntegral sx + sw - 8) < toInteger ( bx + ceiling (1/3 * toRational bw))) || (toInteger (8 + sx) > toInteger ( bx + ceiling (2/3 * toRational bw)))
                        then 31
                        else 0
            y = sy - ymoddifier
            h = sh + fromIntegral ymoddifier
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

## Bindings

### 'Magic' keys

There are so many things that need shortcuts, and so few keys, especially on my tiny keyboard. I try
and keep OS level shortcuts all only on the Super key. But it can be a bit restrictive sometimes.
It's also super annoying when different programs have different shortcuts to do the same thing (why
doesn't every program let me customize shortcuts???). These key bindings try and help with that by
changing based on context. So Super+T always opens a new tab, be it in chrome or in my terminal.
Super+tab and super+shift+tab always swap tabs backward and forward, in programs or in layouts with
tabs. These can be forced to OS versions of the shortcut by adding control. So Super+Control+tab
will always go to the next OS tab, even if I'm focused on chrome or my terminal.

Other 'Magic' keys include window movements. When focused on any none terminal window, super+hjkl
move between window manager windows. If kitty is focused then it will run a script. This script
sends a command to move within kitty windows, and if the focused window didn't change (the movement
is in a direction that would take it out of the terminal window) then moves within window manager
windows instead. It goes a step further by checking if the running process is a Neovim instance and
moving within that if possible (using a modified version of the vim-kitty-navigator extension). This
is done using NVR if the vim instance is the main editor, or by sending keypresses if not.

```haskell
    ("M-h"             ,  bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " moveWindow left h"))
                                    ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " moveWindow left h"))
                                    ,(pure True, windowGo L True)])

```

This is done using the above XMonad key binding and the bellow bash script.

```bash
if [ "$#" -ne "3" ]; then

    kittyStore=$(kitty @ ls)
    commands=$(echo $kittyStore | jq ".[].tabs[].windows[] | select(.is_focused) | .foreground_processes[].cmdline[0]")

    if [[ "$commands" == *"vim"* ]]; then
        titles=$(echo $kittyStore | jq ".[].tabs[].windows[] | select(.is_focused) | .title")

        if [[ "$titles" =~ \"MainEditor\" ]]; then
            tabID=$(echo $kittyStore | jq ".[].tabs[] | select(.is_focused) | .id")
            nvr --servername /tmp/nvr-server-$tabID --nostart -c "KittyNavigate$1"
            exit
        else
            kitty @ send-text ";KittyNavigate$1\n"
            exit
        fi
    fi
fi

old_id=$(kitty @ ls | jq ".[].tabs[].windows[] | select(.is_focused) | .id")

kitty @ kitten neighboring_window.py $1

new_id=$(kitty @ ls | jq ".[].tabs[].windows[] | select(.is_focused) | .id")

if [ "$old_id" -eq "$new_id" ]; then
    xdotool key super+shift+ctrl+$2
fi
```

Super+backspace closes Neovim windows, terminal windows, or window manager windows depending on
what is focused. Super+m moves the window to the master pane, unless the terminal is focused, in
which case it moves the terminal window to the terminal master. If the terminal window is already in
the terminal's master window then it will move to the window manager master (this is super hacky,
relying on the terminal master and stack windows having different dimensions and also relying on a
sleep in the script, I hope to fix this at some point). The tab key also uses a really hacky way to
move in terminal tabs unless there are none, in which case it moves in window manager tabs (still
less hacky than the master moving binding, counting the number of tabs in the focused terminal to
see if there are any to switch to).

Super+enter opens a new terminal (or it should, not working atm for some reason) if there isn't one
on the workspace already, it focuses the terminal if there is, and it opens a new terminal window if
the terminal is already focused. Same for super+b with browsers and browser tabs.

## Other

### Local Modules

ConditionalKeys is from Ethan (where my XMonad config originated from). CycleWSLocal strips out
everything I don't use and adds a function to send the current window to the previous workspace, I
use this a lot now that I'm not using multiple monitors any more (I find it a lot easier than using
numbers to send windows). WindowGoLocal just changes the runOrRaise function to only look on the
current workspace.

# Current Keyboard

My current board is an ez-Planck board. Nothing too special on the mechanical side, just a 40%
ortholinear settup.

## Layers

# Planed Keyboard

Recently I've noticed slight hints of RSI, so I've decided to use it as an excuse to change to a
split keyboard so I can use some tenting. I've started work on a Kyria based keyboard, with the
intent to add a few different switches to get a bit more functionality out of it.

## Hardware

### 5 Way Switch

As much as I like to do everything in Neovim, there are still times that I need the arrow keys (and
even in vim they can make for useful shortcuts). In hopes of moving away from having two dedicated
keys for left and right and duel function keys for up and down, I plan to use a 5 way switch as a
dpad, giving all the arrow keys in the space of one.

### Discrete Rotary Encoder

One of my most used shortcuts is tab and shift tab, so in place of a tab key I'm putting a rotary
wheel. Each click of the rotary encoder ccw will send a tab key, and each cw a shift-tab. This will
allow for easier magic tabbing in xmonad, and for super tabbing in Neovim.

### Smooth Rotary Encoder

The main reason I still reach for my mouse is the scroll wheel, even though in chrome and neovim I
have keyboard shortcuts for it I still feel the need to use the scroll wheel. I'm hoping that the
addition of a scroll wheel to the keyboard will further reduce the use of my mouse.

### Trackball

In addtion to the scroll wheel, having a small trackball on the keyboard should stop jumping to the
mouse for small tasks. I have xmonad update my pointer position based on window swaps, so the cursor
should already be close to where I need it.
