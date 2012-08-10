-module(elibgit).
-behaviour(gen_server).

-export([open/1, get_ref/2, get_tree/2, get_commit/2, get_blob/2, create_blob/2, build_tree/4, create_commit/7, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%
%% elibgit
%% erlang external port bindings for libgit2
%%
%% Copyright (c) 2012, Alex Wilson (alex@uq.edu.au)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of the <organization> nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-include("elibgit.hrl").
-include("git_protocol.hrl").

%% api
%% ===

open(Path) ->
	case gen_server:start_link(?MODULE, [Path], []) of
		{ok, Pid} ->
			{ok, #elibgit{pid = Pid}};
		Other ->
			Other
	end.

get_ref(Ref, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_ref, Ref}).

get_tree(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_tree, Oid}).

get_commit(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_commit, Oid}).

get_blob(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_blob, Oid}).

create_blob(Data, {elibgit, Pid}) ->
	gen_server:call(Pid, {create_blob, Data}).

build_tree(StartingPoint, Removes, Inserts, {elibgit, Pid}) ->
	gen_server:call(Pid, {build_tree, StartingPoint, Removes, Inserts}).

create_commit(Ref, Author, Email, Message, Parents, Tree, {elibgit, Pid}) ->
	gen_server:call(Pid, {create_commit, Ref, Author, Email, Message, Parents, Tree}).

close({elibgit, Pid}) ->
	gen_server:cast(Pid, close).

%% gen_server stuff
%% ===

init([Path]) ->
	process_flag(trap_exit, true),
	case os:find_executable("elibgitd") of
		false ->
			{error, elibgitd_notfound};
		Exec ->
			Port = open_port({spawn_executable, Exec},
						[binary, {packet, 4}, {args, [Path]}]),
			{ok, Port}
	end.

git_type_to_atom(?GIT_OBJ_COMMIT) -> commit;
git_type_to_atom(?GIT_OBJ_TREE) -> tree;
git_type_to_atom(?GIT_OBJ_BLOB) -> blob;
git_type_to_atom(?GIT_OBJ_TAG) -> tag;
git_type_to_atom(Other) -> Other.

tree_binary_to_list(<<"">>, List) ->
	List;
tree_binary_to_list(Bin, List) ->
	<<Type:32/big-signed, Oid:40/binary-unit:8, NameLen:16/big, Rest/binary>> = Bin,
	<<Name:NameLen/binary-unit:8, Rest2/binary>> = Rest,
	Ent = #gitent{name = Name, type = git_type_to_atom(Type), oid = Oid},
	NewList = List ++ [Ent],
	tree_binary_to_list(Rest2, NewList).

check_bin(In) ->
	if is_list(In) ->
		list_to_binary(In);
	is_binary(In) ->
		In;
	true ->
		error({badarg, In})
	end.

check_oid(In) ->
	Bin = check_bin(In),
	if size(Bin) =:= 40 ->
		Bin;
	true ->
		error({badarg, In})
	end.

oid_list(Bin, 0, List) ->
	{List, Bin};
oid_list(Bin, N, List) ->
	<<Oid:40/binary-unit:8, Rest/binary>> = Bin,
	NewList = List ++ [Oid],
	oid_list(Rest, N - 1, NewList).

cat_removes(Bin, []) ->
	Bin;
cat_removes(Bin, List) ->
	[Head | Rest] = List,
	NameBin = check_bin(Head#gitremove.name),
	NameLen = size(NameBin),
	NewBin = <<Bin/binary, NameLen:32/big, NameBin/binary>>,
	cat_removes(NewBin, Rest).

cat_inserts(Bin, []) ->
	Bin;
cat_inserts(Bin, List) ->
	[Head | Rest] = List,
	NameBin = check_bin(Head#gitinsert.name),
	NameLen = size(NameBin),
	OidBin = check_bin(Head#gitinsert.oid),
	Attrs = Head#gitinsert.attrs,
	NewBin = <<Bin/binary, NameLen:32/big, NameBin/binary, OidBin/binary, Attrs:32/big>>,
	cat_inserts(NewBin, Rest).

handle_call({create_commit, Ref, Author, Email, Message, Parents, Tree}, _From, Port) ->
	TreeBin = check_oid(Tree),
	NParents = length(Parents),
	POids = lists:foldl(fun(P, Bin) -> O = check_oid(P), <<Bin/binary, O/binary>> end, <<"">>, Parents),
	Req = <<?OP_CREATECOMMIT:8, TreeBin/binary, NParents:8, POids/binary>>,

	RefBin = check_bin(Ref),
	RefLen = size(RefBin),
	Req2 = <<Req/binary, RefLen:32/big, RefBin/binary>>,

	NameBin = check_bin(Author),
	NameLen = size(NameBin),
	Req3 = <<Req2/binary, NameLen:32/big, NameBin/binary>>,

	EmailBin = check_bin(Email),
	EmailLen = size(EmailBin),
	Req4 = <<Req3/binary, EmailLen:32/big, EmailBin/binary>>,

	MsgBin = check_bin(Message),
	MsgLen = size(MsgBin),
	Req5 = <<Req4/binary, MsgLen:32/big, MsgBin/binary>>,

	Port ! {self(), {command, Req5}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({build_tree, Origin, Removes, Inserts}, _From, Port) ->
	NRemoves = length(Removes),
	NInserts = length(Inserts),
	Req = if is_atom(Origin) ->
		<<?OP_BUILDTREE:8, 0:8>>;
	is_list(Origin) andalso length(Origin) == 40 ->
		OrBin = check_bin(Origin),
		<<?OP_BUILDTREE:8, 1:8, OrBin/binary>>;
	is_binary(Origin) andalso size(Origin) == 40 ->
		<<?OP_BUILDTREE:8, 1:8, Origin/binary>>;
	true ->
		error(badarg)
	end,
	Req2 = <<Req/binary, NRemoves:32/big>>,
	Req3 = cat_removes(Req2, Removes),
	Req4 = <<Req3/binary, NInserts:32/big>>,
	Req5 = cat_inserts(Req4, Inserts),

	Port ! {self(), {command, Req5}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({create_blob, BlobData}, _From, Port) ->
	BData = check_bin(BlobData),
	Port ! {self(), {command, <<?OP_CREATEBLOB:8, BData/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({get_blob, Oid}, _From, Port) ->
	BOid = check_bin(Oid),
	Port ! {self(), {command, <<?OP_GETBLOB:8, BOid/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, Port};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({get_commit, Oid}, _From, Port) ->
	BOid = check_bin(Oid),
	Port ! {self(), {command, <<?OP_GETCOMMIT:8, BOid/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					<<MsgLen:32/big, Rest2/binary>> = Rest,
					<<Msg:MsgLen/binary-unit:8, ParentCount:32/big, Rest3/binary>> = Rest2,
					{Parents, Rest4} = oid_list(Rest3, ParentCount, []),
					<<AuthorLen:32/big, Rest5/binary>> = Rest4,
					<<Author:AuthorLen/binary-unit:8, EmailLen:32/big, Rest6/binary>> = Rest5,
					<<Email:EmailLen/binary-unit:8, TreeOid:40/binary-unit:8>> = Rest6,
					Rec = #gitcommit{msg = Msg, parents = Parents, author = Author, email = Email, tree_oid = TreeOid},
					{reply, {ok, Rec}, Port};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({get_tree, Oid}, _From, Port) ->
	BOid = check_bin(Oid),
	Port ! {self(), {command, <<?OP_GETTREE:8, BOid/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, tree_binary_to_list(Rest, [])}, Port};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end;

handle_call({get_ref, Ref}, _From, Port) ->
	BRef = check_bin(Ref),
	Port ! {self(), {command, <<?OP_GETREF:8, BRef/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, Port};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, Port};
				_ ->
					{reply, {error, unknown}, Port}
			end
	after 1000 ->
		{reply, {error, timeout}, Port}
	end.

handle_cast(close, Port) ->
	Port ! {self(), close},
	receive
		{Port, closed} ->
			{stop, normal, Port}
	end;

handle_cast(_, Port) ->
	{noreply, Port}.

handle_info({'EXIT', Port, _Reason}, Port) ->
	{stop, port_terminated, Port};

handle_info(_, Port) ->
	{noreply, Port}.

terminate(_Reason, _Dicts) ->
	ok.

code_change(_OldVsn, Dicts, _Extra) ->
	{ok, Dicts}.
