-record(user, {
        id      %% 用户ID
        ,name   %% 用户名称
        ,passwd %% 用户登录密码
        ,login_times %% 登录次数
        ,chat_times  %% 聊天次数
        ,last_login  %% 最后一次登录时间
    }
)
