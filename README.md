# Oliver's Config

I'll slowly go through and document some of this stuff I'm not going to go over
every bit of my configs, but it might be worth some of the things I'm doing that
I haven't seen elsewhere (or not as much as I think I should be). I'll try and
give credit to others where it's due, but there's stuff that I can't remember
where it came from.

## NEOVIM

## Repeat Mappings

OK, I love this one. I use a lot of jumping around mappings, bound to ] and [
for forward and backward jumps respectively. Especially treesitter textobject
movements. I have my shift keys bound to send the brackets on a press (but
still send the shift key when held) in my keyboard firmware. Only problem is I
hate having to keep pressing the letters after, makes jumping through a load
of spelling mistakes to correct them all very tedious. So I came up with this
little function. Then in all my mappings for the jumps themselves I add a bit
to overwrite the dirJumps variable like in the example below (note that it's in
which-key.nvim format). I also have all my jump mappings include zz at the end
for that nice screen centeringness.

``` lua
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

I then realized that I could use the same funcion for all kinds of repeats. I've
already added it to my panel opening mappings (all bound under \<leader\>v) so
that \<leader\>vv will open the last panel, or close it if it's still open. This
is just done by having another variable to store the window command in.

All of these also have a default command that is used if nothing has been
called yet (can't repeat what hasn't been done). For jumping the default is to
the next treesitter function. This doesn't make sense for some filetypes, like
latex, so this is used to set a different default for tex files to jump by
section and to open the table of contents instead of the file browser.

``` lua
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
the last command. The teleescope commands come from someone from the neovim
subreddit, the only extra bit is the bit that stores the command (and in the
actual config there are more of them, for picking things like branches).

``` lua
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

### Insert Mappings

#### Snippets and Completion

With a bunch of completion stuff comes a lot of mapping overlaps.
This is how I've tried to fix them.

First up is the usual multi-purpose tab mappings. Nothing too special here, I
don't use tab for scrolling completions (my arrow keys are just too easy to get
to) but I do use it for tabbing out of brackets.

``` lua
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
mappings for opening and closing, so this makes ^space a toggle instaed. This
uses a bit of a hacky approach of first making the compe close command a plug
mapping, but I couldn't get it to work any other way.

``` lua
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

``` vim
inoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')
snoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')
```

Enter is possibly the most used key, used for line breaks, nvim-autopairs
splitting, completion confirmation and changing the choices in luasnip. I've let
autopairs deal with most of it, just adding in a check for luasnip changeability
(and now an extra check for luasnip expandability). I think this might cause
some issues in some snippets, but none that I've used so far.

``` lua
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
 
``` vim
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
```
