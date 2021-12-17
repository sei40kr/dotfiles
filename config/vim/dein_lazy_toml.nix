{ vimPlugins, ... }:

{
  plugins = [
    {
      repo = vimPlugins.clever-f-vim.rtp;
      hook_add = ''
        let g:clever_f_ignore_case = 1
        let g:clever_f_smart_case = 1
        let g:clever_f_not_overwrites_standard_mappings = 1

        nmap f <Plug>(clever-f-f)
        xmap f <Plug>(clever-f-f)
        omap f <Plug>(clever-f-f)
        nmap F <Plug>(clever-f-F)
        xmap F <Plug>(clever-f-F)
        omap F <Plug>(clever-f-F)
        nmap t <Plug>(clever-f-t)
        xmap t <Plug>(clever-f-t)
        omap t <Plug>(clever-f-t)
        nmap T <Plug>(clever-f-T)
        xmap T <Plug>(clever-f-T)
        omap T <Plug>(clever-f-T)
        map ; <Plug>(clever-f-repeat-forward)
        map , <Plug>(clever-f-repeat-back)
      '';
      on_map = "<Plug>(clever-f-";
    }
    {
      repo = vimPlugins.vim-commentary.rtp;
      hook_add = ''
        xmap gc  <Plug>Commentary
        nmap gc  <Plug>Commentary
        omap gc  <Plug>Commentary
        nmap gcc <Plug>CommentaryLine
        if maparg('c', 'n') ==# ''' && !exists('v:operator')
          nmap cgc <Plug>ChangeCommentary
        endif
        nmap gcu <Plug>Commentary<Plug>Commentary
      '';
      on_map = "<Plug>Commentary";
    }
    {
      repo = vimPlugins.vim-easymotion.rtp;
      hook_add = ''
        map gs <Plug>(easymotion-prefix)
      '';
      on_map = "<Plug>(easymotion-prefix)";
    }
    {
      repo = vimPlugins.vim-manpager.rtp;
      hook_source = ''
        autocmd FileType man setlocal nonumber norelativenumber
      '';
      on_cmd = "MANPAGER";
    }
    {
      repo = vimPlugins.vim-pager.rtp;
      on_cmd = "PAGER";
    }
    {
      repo = vimPlugins.vim-surround.rtp;
      hook_add = ''
        let g:surround_no_mappings = 1

        nmap ds  <Plug>Dsurround
        nmap cs  <Plug>Csurround
        nmap cS  <Plug>CSurround
        nmap ys  <Plug>Ysurround
        nmap yS  <Plug>YSurround
        nmap yss <Plug>Yssurround
        nmap ySs <Plug>YSsurround
        nmap ySS <Plug>YSsurround
        xmap S   <Plug>VSurround
        xmap gS  <Plug>VgSurround
      '';
      on_map = [
        "<Plug>Csurround"
        "<Plug>CSurround"
        "<Plug>Dsurround"
        "<Plug>Ysurround"
        "<Plug>YSurround"
        "<Plug>Yssurround"
        "<Plug>YSsurround"
        "<Plug>VSurround"
        "<Plug>VgSurround"
      ];
    }
  ];
}
