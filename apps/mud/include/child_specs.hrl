
%% 该头文件主要是定义各种otp行为里面的子进程启动宏

-ifndef(CHILD_SPECS_HRL).
-define(CHILD_SPECS_HRL, true).


%% 启动一个标准的 supervisor
-define(STD_SUPERVISOR(NN, MM, AA), #{
  id => NN,       % 内部名字
  start => {supervisor, start_link, [{local, NN}, MM, AA]},      % 启动函数
  restart => permanent,
  shutdown => infinity, % 关闭时间，永久等待
  type => supervisor,       % 类型
  modules => [MM]   % 回调模块
}).

%% 启动一个标准的gen_server
-define(STD_GEN_SERVER(NN, MM, AA), #{
  id => NN,       % 名字
  % 上面的id只是内部使用，所以这里的启动还是要加上{local, Name}
  start => {gen_server, start_link, [{local, NN}, MM, AA, []]},      % 启动函数
  restart => permanent,   % 重启策略
  shutdown => 5000, % 关闭时间，单位：毫秒
  type => worker,       % 类型：worker或者supervisor
  modules => [MM]   % 回调模块
}).

%% 启动一个标准的gen_event
-define(STD_GEN_EVENT(NN), #{
  id => NN,       % 名字
  % 上面的id只是内部使用，所以这里的启动还是要加上{local, Name}
  start => {gen_event, start_link, [{local, NN}]},      % 启动函数
  restart => permanent,   % 重启策略
  shutdown => 5000, % 关闭时间，单位：毫秒
  type => worker,       % 类型：worker或者supervisor
  modules => dynamic   % 回调模块
}).


%% 启动一个标准的gen_statem
-define(STD_GEN_STATEM(NN, MM, AA), #{
  id => NN,       % 名字
  % 上面的id只是内部使用，所以这里的启动还是要加上{local, Name}
  start => {gen_statem, start_link, [{local, NN}, MM, AA, []]},      % 启动函数
  restart => permanent,   % 重启策略
  shutdown => 5000, % 关闭时间，单位：毫秒
  type => worker,       % 类型：worker或者supervisor
  modules => [MM]   % 回调模块
}).


%% 启动自定义的gen_server
-define(MUD_SERVER(NN, MM, AA), #{
  id => NN,       % 名字
  % 上面的id只是内部使用，所以这里的启动还是要加上{local, Name}
  start => {gen_server, start_link, [{local, NN}, mud_server, [MM, AA], []]},      % 启动函数
  restart => permanent,   % 重启策略
  shutdown => 5000, % 关闭时间，单位：毫秒
  type => worker,       % 类型：worker或者supervisor
  modules => [mud_server]   % 回调模块
}).



-endif.