{ vimPlugins, ... }:

{
  plugins = [
    {
      repo = vimPlugins.lightline-vim.rtp;
      hook_add = ''
        let g:lightline = { "colorscheme": "onedark" }
      '';
    }
    {
      repo = vimPlugins.onedark-vim.rtp;
      hook_add = ''
        if (has("autocmd") && !has("gui_running"))
            augroup colorset
            autocmd!
            let s:white = { "gui": "#ABB2BF", "cterm": "145", "cterm16" : "7" }
            autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white })
            augroup END
        endif
      '';
      hook_post_source = "colorscheme onedark";
    }
  ];
}
