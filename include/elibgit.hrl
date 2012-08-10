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

-record(elibgit, {pid :: pid()}).

-type elibgit() :: #elibgit{}.
-type git_oid() :: string() | binary().

-record(gitent, {type :: atom(),
				 oid :: git_oid(),
				 name :: string() | binary()}).

-record(gitcommit, {msg :: binary(),
					parents :: [git_oid()],
					author :: binary(),
					email :: binary(),
					tree_oid :: git_oid()}).

-type git_tree_entry() :: #gitent{}.
-type git_commit() :: #gitcommit{}.
-type filename() :: string() | binary().

-record(gitremove, {name :: filename()}).
-record(gitinsert, {name :: filename(),
					  oid :: git_oid(),
					  attrs :: number()}).

-type git_remove_op() :: #gitremove{}.
-type git_insert_op() :: #gitinsert{}.

-spec elibgit:open(Path :: string() | binary()) -> {ok, elibgit()} | {error, term()}.
-spec elibgit:get_ref(Ref :: string() | binary(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
-spec elibgit:get_tree(Oid :: git_oid(), Inst :: elibgit()) -> {ok, [git_tree_entry()]} | {error, term()}.
-spec elibgit:get_commit(Oid :: git_oid(), Inst :: elibgit()) -> {ok, git_commit()} | {error, term()}.
-spec elibgit:get_blob(Oid :: git_oid(), Inst :: elibgit()) -> {ok, binary()} | {error, term()}.
-spec elibgit:create_blob(Data :: binary(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
-spec elibgit:build_tree(StartingPoint :: git_oid() | empty, Removes :: [git_remove_op()], Inserts :: [git_insert_op()], Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
-spec elibgit:create_commit(Ref :: filename(), Author :: string() | binary(), Email :: string() | binary(), Message :: string() | binary(), Parents :: [git_oid()], Tree :: git_oid(), Inst :: elibgit()) -> {ok, git_oid()} | {error, term()}.
-spec elibgit:close(Inst :: elibgit()) -> ok | {error, term()}.
