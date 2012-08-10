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

%% operation numbers for talking to the driver
-define(OP_GETREF, 			 1).
-define(OP_GETTREE,			 2).
-define(OP_GETCOMMIT,		 3).
-define(OP_GETBLOB,			 4).
-define(OP_BUILDTREE,		 5).
-define(OP_CREATEBLOB,		 6).
-define(OP_CREATECOMMIT,	 7).

%% git error constants, from <git2/errors.h>
-define(GIT_OK, 			 0).
-define(GIT_ERROR, 			-1).
-define(GIT_ENOTFOUND, 		-3).
-define(GIT_EEXISTS, 		-4).
-define(GIT_EAMBIGUOUS, 	-5).
-define(GIT_EBUFS,			-6).
-define(GIT_EUSER, 			-7).
-define(GIT_PASSTHROUGH, 	-30).
-define(GIT_REVWALKOVER, 	-31).

%% git tree entry types
-define(GIT_OBJ_ANY,		-2).
-define(GIT_OBJ_BAD,		-1).
-define(GIT_OBJ_COMMIT,		 1).
-define(GIT_OBJ_TREE,		 2).
-define(GIT_OBJ_BLOB,		 3).
-define(GIT_OBJ_TAG,		 4).
