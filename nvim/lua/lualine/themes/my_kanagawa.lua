local theme = require("kanagawa.colors").setup().theme

local kanagawa = {}

kanagawa.normal = {
  a = { bg = theme.syn.fun, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.syn.fun },
  c = { bg = theme.ui.bg, fg = theme.ui.fg },
}

kanagawa.insert = {
  a = { bg = theme.diag.ok, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.diag.ok },
}

kanagawa.command = {
  a = { bg = theme.syn.operator, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.syn.operator },
}

kanagawa.visual = {
  a = { bg = theme.syn.keyword, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.syn.keyword },
}

kanagawa.replace = {
  a = { bg = theme.syn.constant, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.syn.constant },
}

kanagawa.terminal = {
  a = { bg = theme.syn.constant, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.syn.constant },
}

kanagawa.inactive = {
  a = { bg = theme.syn.comment, fg = theme.ui.bg },
  b = { bg = theme.diff.change, fg = theme.ui.fg_dim },
  c = { bg = theme.ui.bg, fg = theme.ui.fg_dim },

  x = { bg = theme.ui.bg, fg = theme.ui.fg_dim },
  y = { bg = theme.diff.change, fg = theme.ui.fg_dim },
  z = { bg = theme.syn.comment, fg = theme.ui.bg },
}

if vim.g.kanagawa_lualine_bold then
  for _, mode in pairs(kanagawa) do
    mode.a.gui = "bold"
  end
end

return kanagawa
