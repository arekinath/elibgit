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

%% @doc This module contains methods for working with a Git respository instance.
%%
%% The repository instance is created with the {@link elibgit:open/1.} function,
%% and then can be used with subscript notation, like so:
%% <blockquote><pre>
%% {ok, Git} = elibgit:open(Path),
%% {ok, Oid} = Git:get_ref("...").
%% </pre></blockquote>
%%
%% Once you are finished with an instance, you should call the {@link elibgit:close/1.}
%% function to close the port and clean up resources.
-module(elibgit).
-behaviour(gen_server).

-export([open/1, get_ref/2, get_tree/2, get_commit/2, get_blob/2, create_blob/2, build_tree/4, create_commit/7, close/1, read_file/3, write_files/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("elibgit.hrl").
-include("git_protocol.hrl").

%% api
%% ===

%% @doc Open a new elibgit instance.
%%
%% Path should contain the full path to the repository.
-spec elibgit:open(Path :: string() | binary()) -> {ok, elibgit()} | {error, term()}.
open(Path) ->
	case gen_server:start_link(?MODULE, [Path], []) of
		{ok, Pid} ->
			{ok, #elibgit{pid = Pid}};
		Other ->
			Other
	end.

%% @doc Resolves a 'ref' in the repository.
%%
%% This should be the fully qualified reference name, e.g. <code>refs/heads/master</code>, not
%% just <code>master</code>.
-spec elibgit:get_ref(Ref :: string() | binary(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
get_ref(Ref, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_ref, Ref}, 20000).


%% @doc Retrieves a tree object.
-spec elibgit:get_tree(Oid :: git_oid(), Inst :: elibgit()) -> {ok, [git_tree_entry()]} | {error, term()}.
get_tree(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_tree, Oid}, 20000).


%% @doc Retrieves a commit object.
-spec elibgit:get_commit(Oid :: git_oid(), Inst :: elibgit()) -> {ok, git_commit()} | {error, term()}.
get_commit(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_commit, Oid}, 20000).

%% @doc Retrieves the data inside a blob object.
-spec elibgit:get_blob(Oid :: git_oid(), Inst :: elibgit()) -> {ok, binary()} | {error, term()}.
get_blob(Oid, {elibgit, Pid}) ->
	gen_server:call(Pid, {get_blob, Oid}, 20000).

%% @doc Creates a new blob in the respository, returning its new OID.
-spec elibgit:create_blob(Data :: binary(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
create_blob(Data, {elibgit, Pid}) ->
	gen_server:call(Pid, {create_blob, Data}, 32000).

%% @doc Builds a new tree using a basis and list of operations.
%%
%% The new tree is initialised with identical contents to the StartingPoint tree. Then,
%% the sub-trees and blobs specified in Removes are deleted, in order. Finally, the
%% new sub-trees and blobs in Inserts are added in order, and the resulting tree
%% written to the repository.
%%
%% For example, to take the existing tree <code>"abcd1234..."</code> and add a new file,
%% <code>"bah.txt"</code>, you would first call {@link create_blob/2.} with the data
%% to go inside it. Then, call <code>build_tree</code> with the StartingPoint as
%% <code>"abcd1234..."</code>, no Remove operations, and one Insert operation:
%% <code>{gitinsert, "bah.txt", "blob_oid_here", 0}</code>.
-spec elibgit:build_tree(StartingPoint :: git_oid() | empty, Removes :: [git_remove_op()], Inserts :: [git_insert_op()], Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
build_tree(StartingPoint, Removes, Inserts, {elibgit, Pid}) ->
	gen_server:call(Pid, {build_tree, StartingPoint, Removes, Inserts}, 20000).

%% @doc Creates a new commit and updates a ref to point at it.
%%
%% Note that for non-bare repositories, updating the ref of the current branch like this
%% is a really Bad Idea.
-spec elibgit:create_commit(Ref :: filename(), Author :: string() | binary(), Email :: string() | binary(), Message :: string() | binary(), Parents :: [git_oid()], Tree :: git_oid(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
create_commit(Ref, Author, Email, Message, Parents, Tree, {elibgit, Pid}) ->
	gen_server:call(Pid, {create_commit, Ref, Author, Email, Message, Parents, Tree}, 20000).

%% @doc Closes a repository instance.
-spec elibgit:close(Inst :: elibgit()) -> ok | {error, term()}.
close({elibgit, Pid}) ->
	gen_server:cast(Pid, close).

% Turns a path into a list of {Name, GitOid} tuples.
-spec path_to_trees(Repo :: elibgit(), Tree :: [#gitent{}], Path :: [string()], Initial :: []) -> [{string(), git_oid()}].
path_to_trees(_Repo, _Tree, [_File], SoFar) ->
	SoFar;
path_to_trees(Repo, Tree, [Next | Rest], SoFar) ->
	NextBin = list_to_binary(Next),
	case lists:filter(fun(Ent) -> (Ent#gitent.type =:= tree) andalso (Ent#gitent.name =:= NextBin) end, Tree) of
		[SubtreeEnt] ->
			{ok, Subtree} = Repo:get_tree(SubtreeEnt#gitent.oid),
			NewSoFar = [{Next, SubtreeEnt#gitent.oid}] ++ SoFar,
			path_to_trees(Repo, Subtree, Rest, NewSoFar);
		_ ->
			NewSoFar = [{Next, none}] ++ SoFar,
			path_to_trees(Repo, [], Rest, NewSoFar)
	end.

% Recursively constructs a tree back to the root.
-spec new_tree_with(Repo :: elibgit(), LastName :: string(), LastOid :: git_oid(), Attrs :: integer(), Path :: [{Name :: string(), git_oid()}]) -> git_oid().
new_tree_with(_, _, LastOid, _, []) ->
	LastOid;
new_tree_with(Repo, LastName, LastOid, Attrs, [Next | Rest]) ->
	{Name, OldOid} = Next,
	{ok, NewOid} = if LastOid =:= delete ->
		Repo:build_tree(OldOid, [#gitremove{name = LastName}], []);
	true ->
		Repo:build_tree(OldOid, [], [#gitinsert{name = LastName, oid = LastOid, attrs = Attrs}])
	end,
	%error_logger:info_report([{build_from, OldOid}, {with, {LastName, LastOid}}, {makes, NewOid}]),
	new_tree_with(Repo, Name, NewOid, 8#040000, Rest).

% Builds a new tree with the data at Path replaced by Data.
-spec build_tree_path(Repo :: elibgit(), BaseTree :: git_oid(), Path :: [string()], Data :: delete | binary()) -> git_oid().
build_tree_path(Repo, TreeOid, Path, Data) ->
	{ok, Tree} = Repo:get_tree(TreeOid),
	PathOids = path_to_trees(Repo, Tree, Path, []) ++ [{"root", TreeOid}],
	if is_atom(Data) ->
		new_tree_with(Repo, lists:last(Path), delete, 0, PathOids);
	true ->
		{ok, BlobOid} = Repo:create_blob(Data),
		new_tree_with(Repo, lists:last(Path), BlobOid, 8#100644, PathOids)
	end.

%% @doc High-level function for reading a file in a tree by its path
%%
%% This function walks down a tree to a given relative path and retrieves
%% the blob object stored there.
-spec elibgit:read_file(Tree :: git_oid(), Path :: [string()], Inst :: elibgit()) -> {ok, binary()} | {error, term()}.
read_file(TreeOid, Path, Repo = {elibgit, _Pid}) ->
	case (catch (begin
		Kids = case lists:reverse(Path) of
			[Name] ->
				{ok, Tree} = Repo:get_tree(TreeOid),
				Tree;
			[Name | Rest] ->
				{ok, Tree} = Repo:get_tree(TreeOid),
				lists:foldr(fun(DirName, Subtree) ->
					case lists:filter(fun(Ent) -> (Ent#gitent.type =:= tree) andalso (Ent#gitent.name =:= list_to_binary(DirName)) end, Subtree) of
						[#gitent{oid = NewOid}] ->
							{ok, NewSubtree} = Repo:get_tree(NewOid),
							NewSubtree;
						_ ->
							error({not_found, DirName})
					end
				end, Tree, Rest)
		end,
		case lists:filter(fun(Ent) -> Ent#gitent.name =:= list_to_binary(lists:last(Path)) end, Kids) of
			[#gitent{type = blob, oid = BlobOid}] ->
				Repo:get_blob(BlobOid);
			[#gitent{}] ->
				{error, not_blob};
			_ ->
				{error, {not_found, lists:last(Path)}}
		end
	end)) of
		{'EXIT', Reason} ->
			{error, Reason};
		Other ->
			Other
	end.

%% @doc High-level function for writing files in a tree by their path
%%
%% This function takes a base tree and writes files at a given relative path
%% with new data, returning a new tree with changes made.
-spec elibgit:write_files(BaseTree :: git_oid(), Files :: [{Path :: [string()], Data :: delete | binary()}], Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
write_files(InTree, Files, Repo = {elibgit, _Pid}) ->
	case (catch (lists:foldl(fun({Path, Data}, Tree) ->
		build_tree_path(Repo, Tree, Path, Data)
	end, InTree, Files))) of
		{'EXIT', Reason} ->
			{error, Reason};
		Oid ->
			{ok, Oid}
	end.

%% gen_server stuff
%% ===

-record(state, {port, exec, path}).

%% @private
init([Path]) ->
	process_flag(trap_exit, true),
	OsPath = case os:getenv("PATH") of
		false ->
			[".", "./priv"];
		OtherOs ->
			[".", "./priv", OtherOs]
	end,
	WithPriv = case code:priv_dir(elibgit) of
		{error, _} ->
			OsPath;
		Other ->
			[Other] ++ OsPath
	end,
	FindPath = lists:foldl(fun(P, Acc) ->
		if length(Acc) =:= 0 ->
			P;
		true ->
			P ++ ":" ++ Acc
		end
	end, "", WithPriv),
	case os:find_executable("elibgitd", FindPath) of
		false ->
			{error, elibgitd_notfound};
		Exec ->
			Port = open_port({spawn_executable, Exec},
						[binary, {packet, 4}, {args, [Path]}]),
			{ok, #state{port=Port,exec=Exec,path=Path}}
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

%% @private
handle_call({create_commit, Ref, Author, Email, Message, Parents, Tree}, _From, State) ->
	Port = State#state.port,

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
					{reply, {ok, Rest}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({build_tree, Origin, Removes, Inserts}, _From, State) ->
	Port = State#state.port,

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
					{reply, {ok, Rest}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({create_blob, BlobData}, _From, State) ->
	Port = State#state.port,
	BData = check_bin(BlobData),
	Port ! {self(), {command, <<?OP_CREATEBLOB:8, BData/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 30000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({get_blob, Oid}, _From, State) ->
	Port = State#state.port,
	BOid = check_bin(Oid),
	Port ! {self(), {command, <<?OP_GETBLOB:8, BOid/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, State};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({get_commit, Oid}, _From, State) ->
	Port = State#state.port,
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
					{reply, {ok, Rec}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({get_tree, Oid}, _From, State) ->
	Port = State#state.port,
	BOid = check_bin(Oid),
	Port ! {self(), {command, <<?OP_GETTREE:8, BOid/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, tree_binary_to_list(Rest, [])}, State};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end;

handle_call({get_ref, Ref}, _From, State) ->
	Port = State#state.port,
	BRef = check_bin(Ref),
	Port ! {self(), {command, <<?OP_GETREF:8, BRef/binary>>}},
	receive
		{Port, {data, Data}} ->
			<<ErrCode:32/big-signed, Rest/binary>> = Data,
			case ErrCode of
				?GIT_OK ->
					{reply, {ok, Rest}, State};
				?GIT_ENOTFOUND ->
					{reply, {error, notfound}, State};
				?ERROR_STRERROR ->
					<<Errno:32/big-signed, Message/binary>> = Rest,
					{reply, {error, Errno, Message}, State};
				_ ->
					{reply, {error, unknown}, State}
			end
	after 1000 ->
		{reply, {error, timeout}, State}
	end.

%% @private
handle_cast(close, State) ->
	Port = State#state.port,
	Port ! {self(), close},
	receive
		{Port, closed} ->
			{stop, normal, State}
	end;

handle_cast(_, State) ->
	{noreply, State}.

%% @private
handle_info({'EXIT', _Port, _Reason}, State) ->
	#state{exec=Exec,path=Path} = State,
	NewPort = open_port({spawn_executable, Exec},
						[binary, {packet, 4}, {args, [Path]}]),
	{noreply, State#state{port=NewPort}};

handle_info(_, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
