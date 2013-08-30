-module(re_test).
-compile(export_all).

remove_html_tag(Data) ->
    remove_html_tag(Data, 0).

remove_html_tag(Data, Offset) ->
    case re:run(Data, "<.*?>", [dotall, {capture, first, index}, {offset, Offset}]) of
        nomatch ->
            Data;
        {match, [{Index, _Len}]} ->
            remove_html_tag(re:replace(Data, "<.*?>", "", [dotall, {return, list}]), Index)
    end.
