<!-- markdownlint-disable MD013 -->
# Oliver's Config

I'll slowly go through and document some of this stuff I'm not going to go over
every bit of my configs, but it might be worth some of the things I'm doing that
I haven't seen elsewhere (or not as much as I think I should be). I'll try and
give credit to others where it's due, but there's stuff that I can't remember
where it came from.

## Neovim

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
for that nice screen centeringness and m\` to add it to the jump list (I think
it should be m', but that doesn't seem to work)

```lua
vim.api.nvim_set_var("dirJumps", "f")

function _G.commandRepeat(leader, varName)
    local jump = vim.api.nvim_get_var(varName)
    return vim.api.nvim_replace_termcodes(leader .. jump, true, true, true)
end

    ["]"] = {
        ["]"] = { "v:lua.commandRepeat(']', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
        f = { "<cmd>let g:dirJumps='f'<cr><cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Function" },
    }
```

Realizing that I could use the same function for all kinds of repeats. I've
already added it to my panel opening mappings (all bound under \<leader\>v) so
that \<leader\>vv will open the last panel, or close it if it's still open. This
is just done by having another variable to store the window command in.

All of these also have a default command that is used if nothing has been
called yet (can't repeat what hasn't been done). For jumping the default is to
the next treesitter function. This doesn't make sense for some file types, like
latex, so this is used to set a different default for Tex files to jump by
section and to open the table of contents instead of the file browser.

```lua
if vim.api.nvim_get_var("dirJumps") == "f" then
    vim.api.nvim_set_var("dirJumps", "s")
end
if vim.api.nvim_get_var("panelRepeat") == "x" then
    vim.api.nvim_set_var("panelRepeat", "c")
end
```

## Diffview Mappings

I have a few commands to let me pick what to diff against in diffview, so I
added a variation of the above to give me a mapping of reopening diffview using
the last command. The telescope commands come from someone from the Neovim
subreddit, the only extra bit is the bit that stores the command (and in the
actual config there are more of them, for picking things like branches).

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
numbers can still be used for fast jumping. In addition they also add to the
jump list if a count greater than 5 is given.

```vim
nnoremap <expr> j v:count?(v:count>5?"m'".v:count:'').'j':'gj'
nnoremap <expr> k v:count?(v:count>5?"m'".v:count:'').'k':'gk'
```

It makes sense to me to have 'big' h and l do a bigger version of he h or
l movement. So this mapping makes H and L go to the start and end of the
line respectively. It's a little more than that though, they go to the first
or last non-whitespace character. If the cursor is already on the first or
last non-whitespace character then it instead gos to the true first or last
character.

```vim
nnoremap <expr> H getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^'
nnoremap <expr> L getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_'
```

## Insert Mappings

#### Snippets and Completion

With a bunch of completion stuff comes a lot of mapping overlaps.
This is how I've tried to fix them.

First up is the usual multi-purpose tab mappings. Nothing too special here, I
don't use tab for scrolling completions (my arrow keys are just too easy to get
to) but I do use it for tabbing out of brackets.

```lua
local luasnip = require("luasnip")
_G.tab_complete = function()
    if luasnip and luasnip.expand_or_jumpable() then
        return replace_keycodes("<Plug>luasnip-expand-or-jump")
    else
        return replace_keycodes("<plug>(TaboutMulti)")
    end
end

_G.s_tab_complete = function()
    if luasnip and luasnip.jumpable(-1) then
        return replace_keycodes("<Plug>luasnip-jump-prev")
    else
        return replace_keycodes("<plug>(TaboutBackMulti)")
    end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
```

Next up is toggling of the completion pop-up. I didn't want two separate
mappings for opening and closing, so this makes control space a toggle instead. This
uses a bit of a hacky approach of first making the compe close command a plug
mapping, but I couldn't get it to work any other way.

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

Enter is possibly the most used key, used for line breaks, nvim-autopairs
splitting, completion confirmation and changing the choices in luasnip. I've let
autopairs deal with most of it, just adding in a check for luasnip changeability
(and now an extra check for luasnip expandability). I think this might cause
some issues in some snippets, but none that I've used so far.

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

This mapping takes the word from above (c-y) or below (c-l) the cursor. Credit
goes to someone online for the control y mapping, I've simply extended it to
work from below as well.

```vim
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
```

## XMonad

### Notebook Layout

OK, so this has a bit of a weird name, it came from the original use case and I
just can't be bothered to think of a better one. So the original use case, I
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
be tabbed on top of each other, and so on for the column and tabbed windows. My
motivation to work on this right now is low as it is only used on my second
monitor, but if I ever move to a laptop then it will be killer. I'll have to get
my head around the logic of the persistent two pane layouts and try and nick that.

I've also got it set up with sub-layout, so I can have tabs on the main layout
for those D&D games (keep character sheets that won't appear at the same time
in a tab group to save a bit of space), but it also works nicely when changing
screens.

I love this layout so much that not only is it basically all I use, but I also
use it twice. In that I have two different layouts defined that use the notebook
layout just with different default settings.

### Toggleable layouts

The only non notebook layouts I actually use are the three toggleable layouts
I have set up. The first one is toggleable full-screen. A fairly standard one
that lets me have the best of both worlds for full-screening, allowing me to have
a contained maximize within a window and then choose to make it actually full
screen.

Fullbar takes all the windows on a workspace and tabs them, so that it's kind of
full screen but keeping the status bar and also having a way of seeing what's
open. Centre focus is the same, but with big gaps on the sides. This is great
for big monitors, being able to keep the thing you're looking at right in front
of you instead of way off to one side. I have this set up per workspace, so
normally it defaults to half the screen width, but on my thesis workspace it
instead uses roughly the width of 100 character columns in my terminal. For that
extra focused writing experience.

### 'Magic' keys

There are so many things that need shortcuts, and so few keys, especially on my
tiny keyboard. I try and keep OS level shortcuts all only on the Super key. But
it can be a bit restrictive sometimes. It's also super annoying when different
programs have different shortcuts to do the same thing (why doesn't every
program let me customize shortcuts???). These key bindings try and help with
that by changing based on context. So Super+T always opens a new tab, be it in
chrome or in my terminal. Super+left and super+right always swap tabs backward
and forward, in programs or in layouts with tabs. These can be forced to OS
versions of the shortcut by adding control. So Super+Control+right will always
go to the next OS tab, even if I'm focused on chrome or my terminal.

I would like to take this a step further by adding moving by direction, so it
will move through window windows, terminal windows and Neovim windows, but I
will need to learn how to extend Kitty first.
