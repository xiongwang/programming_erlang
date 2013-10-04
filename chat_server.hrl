-record(user, {
         name           %% 用户名称
        ,passwd         %% 用户登录密码
        ,login_times= 0 %% 登录次数
        ,chat_times = 0 %% 聊天次数
        ,last_login = 0 %% 最后一次登录时间
    }
).
