/*
* elibgit
* erlang external port bindings for libgit2
*
* Copyright (c) 2012, Alex Wilson (alex@uq.edu.au)
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <git2.h>
#include <stdint.h>
#include <string.h>

/* for htonl */
#include <arpa/inet.h>

#define OP_GETREF			1
#define OP_GETTREE			2
#define OP_GETCOMMIT		3
#define OP_GETBLOB			4
#define OP_BUILDTREE		5
#define OP_CREATEBLOB		6
#define OP_CREATECOMMIT		7

void
write_error(int err)
{
	uint32_t tlen;
	tlen = htonl(sizeof(int));
	write(1, &tlen, sizeof(uint32_t));

	err = htonl(err);
	write(1, &err, sizeof(int));
}

int
read_oid(git_oid *oid, uint32_t len)
{
	char *buf;
	int err;

	buf = malloc(len + 1);
	if (buf == NULL)
		exit(2);
	if (read(0, buf, len) != (len))
		exit(2);
	buf[len] = 0;

	if ((err = git_oid_fromstr(oid, buf))) {
		free(buf);
		return err;
	}

	free(buf);
	return 0;
}

uint32_t
read_string(char **buf)
{
	uint32_t len;
	if (read(0, &len, sizeof(len)) != sizeof(len))
		exit(2);
	len = ntohl(len);
	*buf = malloc(len + 1);
	if (*buf == NULL)
		exit(2);
	if (read(0, *buf, len) != len)
		exit(2);
	buf[0][len] = 0;

	return len;
}

void
handle_createcommit(git_repository *repo, uint32_t inlen)
{
	int err, i;
	uint32_t len;
	char *name = NULL, *email = NULL, *message = NULL, *ref = NULL;
	git_oid oid;
	git_oid tree_oid;
	git_tree *tree = NULL;
	git_signature *sig = NULL;
	uint8_t parents;
	git_oid parent_oid[3];
	git_commit *parent[3] = {NULL};

	if ((err = read_oid(&tree_oid, GIT_OID_HEXSZ))) {
		write_error(err);
		goto done;
	}

	if ((err = git_tree_lookup(&tree, repo, &tree_oid))) {
		write_error(err);
		return;
	}

	if (read(0, &parents, sizeof(parents)) != sizeof(parents))
		exit(2);

	for (i = 0; i < parents; ++i) {
		if ((err = read_oid(&parent_oid[i], GIT_OID_HEXSZ))) {
			write_error(err);
			goto done;
		}

		if ((err = git_commit_lookup(&parent[i], repo, &parent_oid[i]))) {
			write_error(err);
			goto done;
		}
	}

	read_string(&ref);
	read_string(&name);
	read_string(&email);

	if ((err = git_signature_now(&sig, name, email))) {
		write_error(err);
		goto done;
	}

	read_string(&message);

	if ((err = git_commit_create(&oid, repo, ref, sig, sig,
			NULL, message, tree, parents, (const git_commit**)&parent))) {
		write_error(err);
		goto done;
	}

	len = htonl(sizeof(int) + GIT_OID_HEXSZ);
	err = 0;

	write(1, &len, sizeof(len));
	write(1, &err, sizeof(err));

	char toutbuf[GIT_OID_HEXSZ+1];
	git_oid_tostr(toutbuf, sizeof(toutbuf), &oid);
	write(1, toutbuf, GIT_OID_HEXSZ);

done:
	if (ref) free(ref);
	if (name) free(name);
	if (email) free(email);
	if (message) free(message);
	if (sig) git_signature_free(sig);
	if (tree) git_tree_free(tree);
	for (i = 0; i < 3; ++i)
		if (parent[i]) git_commit_free(parent[i]);
}

void
handle_createblob(git_repository *repo, uint32_t inlen)
{
	int err;
	char *buf;
	uint32_t len;

	buf = malloc(inlen);
	if (buf == NULL)
		exit(2);
	if (read(0, buf, inlen) != inlen)
		exit(2);

	git_oid oid;
	if ((err = git_blob_create_frombuffer(&oid, repo, buf, inlen))) {
		write_error(err);
		free(buf);
		return;
	}

	len = htonl(sizeof(int) + GIT_OID_HEXSZ);
	err = 0;

	write(1, &len, sizeof(len));
	write(1, &err, sizeof(err));

	char toutbuf[GIT_OID_HEXSZ+1];
	git_oid_tostr(toutbuf, sizeof(toutbuf), &oid);
	write(1, toutbuf, GIT_OID_HEXSZ);

	free(buf);
}

void
handle_buildtree(git_repository *repo, uint32_t inlen)
{
	int err, i;
	uint32_t count, len;
	char useorigin = 1;
	char *buf = NULL;
	git_tree *origin = NULL;
	git_treebuilder *builder = NULL;

	if (read(0, &useorigin, sizeof(useorigin)) != sizeof(useorigin))
		exit(2);

	if (useorigin) {
		git_oid origin_oid;

		if ((err = read_oid(&origin_oid, GIT_OID_HEXSZ))) {
			write_error(err);
			return;
		}

		if ((err = git_tree_lookup(&origin, repo, &origin_oid))) {
			write_error(err);
			return;
		}
	}

	if ((err = git_treebuilder_create(&builder, origin))) {
		write_error(err);
		goto done;
	}

	/* do remove operations */
	if (read(0, &count, sizeof(count)) != sizeof(count))
		exit(2);
	count = ntohl(count);

	for (i = 0; i < count; ++i) {
		if (read(0, &len, sizeof(len)) != sizeof(len))
			exit(2);
		len = ntohl(len);

		buf = malloc(len+1);
		if (buf == NULL)
			exit(2);
		if (read(0, buf, len) != len)
			exit(2);
		buf[len] = 0;

		if ((err = git_treebuilder_remove(builder, buf))) {
			write_error(err);
			goto done;
		}

		free(buf);
		buf = NULL;
	}

	/* do insert operations */
	if (read(0, &count, sizeof(count)) != sizeof(count))
		exit(2);
	count = ntohl(count);

	for (i = 0; i < count; ++i) {
		if (read(0, &len, sizeof(len)) != sizeof(len))
			exit(2);
		len = ntohl(len);

		buf = malloc(len+1);
		if (buf == NULL)
			exit(2);
		if (read(0, buf, len) != len)
			exit(2);
		buf[len] = 0;

		git_oid oid;
		if ((err = read_oid(&oid, GIT_OID_HEXSZ))) {
			write_error(err);
			goto done;
		}

		uint32_t attrs;
		if (read(0, &attrs, sizeof(attrs)) != sizeof(attrs))
			exit(2);
		attrs = ntohl(attrs);

		if ((err = git_treebuilder_insert(NULL, builder, buf, &oid, attrs))) {
			write_error(err);
			goto done;
		}

		free(buf);
		buf = NULL;
	}

	git_oid outid;
	if ((err = git_treebuilder_write(&outid, repo, builder))) {
		write_error(err);
		goto done;
	}

	len = htonl(sizeof(int) + GIT_OID_HEXSZ);
	err = 0;

	write(1, &len, sizeof(len));
	write(1, &err, sizeof(err));

	char toutbuf[GIT_OID_HEXSZ+1];
	git_oid_tostr(toutbuf, sizeof(toutbuf), &outid);
	write(1, toutbuf, GIT_OID_HEXSZ);

done:
	if (origin)
		git_tree_free(origin);
	if (builder)
		git_treebuilder_free(builder);
	if (buf)
		free(buf);
}

void
handle_getblob(git_repository *repo, uint32_t inlen)
{
	git_oid oid;
	git_blob *blob;
	int err;
	uint32_t tlen;

	if ((err = read_oid(&oid, inlen))) {
		write_error(err);
		return;
	}

	if ((err = git_blob_lookup(&blob, repo, &oid))) {
		write_error(err);
		return;
	}

	tlen = htonl(git_blob_rawsize(blob) + sizeof(int));
	write(1, &tlen, sizeof(tlen));

	err = 0;
	write(1, &err, sizeof(err));

	write(1, git_blob_rawcontent(blob), git_blob_rawsize(blob));

	git_blob_free(blob);
}

void
handle_getcommit(git_repository *repo, uint32_t inlen)
{
	git_oid oid;
	git_commit *commit;
	int err, i;
	uint32_t tlen, len;

	if ((err = read_oid(&oid, inlen))) {
		write_error(err);
		return;
	}

	if ((err = git_commit_lookup(&commit, repo, &oid))) {
		write_error(err);
		return;
	}

	/* calculate the total size first */
	tlen = sizeof(int); /* error status */

	tlen += sizeof(int); /* message length */
	tlen += strlen(git_commit_message(commit)); /* message */

	tlen += sizeof(int); /* parent count */
	tlen += git_commit_parentcount(commit) * GIT_OID_HEXSZ; /* parent oids */

	const git_signature *sig = git_commit_author(commit);
	tlen += sizeof(int); /* author name len */
	tlen += strlen(sig->name);
	tlen += sizeof(int); /* author email len */
	tlen += strlen(sig->email);

	tlen += GIT_OID_HEXSZ; /* tree oid */

	/* now actually write it all */
	tlen = htonl(tlen);
	write(1, &tlen, sizeof(tlen));

	err = 0;
	write(1, &err, sizeof(err));

	len = htonl(strlen(git_commit_message(commit)));
	write(1, &len, sizeof(len));
	write(1, git_commit_message(commit), ntohl(len));

	len = htonl(git_commit_parentcount(commit));
	write(1, &len, sizeof(len));

	for (i = 0; i < git_commit_parentcount(commit); ++i) {
		const git_oid *oid = git_commit_parent_oid(commit, i);
		char outbuf[GIT_OID_HEXSZ+1];
		git_oid_tostr(outbuf, sizeof(outbuf), oid);
		write(1, outbuf, GIT_OID_HEXSZ);
	}

	len = htonl(strlen(sig->name));
	write(1, &len, sizeof(len));
	write(1, sig->name, ntohl(len));

	len = htonl(strlen(sig->email));
	write(1, &len, sizeof(len));
	write(1, sig->email, ntohl(len));

	const git_oid *toid = git_commit_tree_oid(commit);
	char toutbuf[GIT_OID_HEXSZ+1];
	git_oid_tostr(toutbuf, sizeof(toutbuf), toid);
	write(1, toutbuf, GIT_OID_HEXSZ);

	git_commit_free(commit);
}

void
handle_gettree(git_repository *repo, uint32_t inlen)
{
	git_oid oid;
	git_tree *tree;
	int32_t err;
	uint32_t tlen;
	int i;

	if ((err = read_oid(&oid, inlen))) {
		write_error(err);
		return;
	}

	if ((err = git_tree_lookup(&tree, repo, &oid))) {
		write_error(err);
		return;
	}

	uint32_t count = git_tree_entrycount(tree);
	tlen = sizeof(int); /* error code */
	for (i = 0; i < count; ++i) {
		const git_tree_entry *ent = git_tree_entry_byindex(tree, i);
		tlen += sizeof(int); /* type */
		tlen += GIT_OID_HEXSZ; /* oid */
		tlen += sizeof(uint16_t); /* length of name */
		tlen += strlen(git_tree_entry_name(ent)); /* name */
	}

	tlen = htonl(tlen);
	write(1, &tlen, sizeof(tlen));

	err = 0;
	write(1, &err, sizeof(err));

	for (i = 0; i < count; ++i) {
		const git_tree_entry *ent = git_tree_entry_byindex(tree, i);
		int type = htonl(git_tree_entry_type(ent));
		write(1, &type, sizeof(type));

		char outbuf[GIT_OID_HEXSZ+1];
		git_oid_tostr(outbuf, sizeof(outbuf), git_tree_entry_id(ent));
		write(1, outbuf, GIT_OID_HEXSZ);

		const char *name = git_tree_entry_name(ent);
		uint16_t namelen = htons(strlen(name));
		write(1, &namelen, sizeof(namelen));
		write(1, name, strlen(name));
	}

	git_tree_free(tree);
}

void
handle_getref(git_repository *repo, uint32_t inlen)
{
	git_reference *ref = NULL;
	int32_t err;
	uint32_t len, tlen;
	char *buf;
	const git_oid *oid;

	buf = malloc(inlen + 1);
	if (buf == NULL)
		exit(2);
	if (read(0, buf, inlen) != inlen)
		exit(2);
	buf[inlen] = 0;

	if ((err = git_reference_lookup(&ref, repo, buf))) {
		write_error(err);
		return;
	}

	if ((oid = git_reference_oid(ref)) == NULL)
		exit(2);

	char outbuf[GIT_OID_HEXSZ+1];
	git_oid_tostr(outbuf, sizeof(outbuf), oid);

	len = strlen(outbuf);

	tlen = htonl(sizeof(int32_t) + len);
	write(1, &tlen, sizeof(uint32_t));

	err = htonl(err);
	write(1, &err, sizeof(int32_t));

	write(1, outbuf, len);

	free(buf);
	git_reference_free(ref);
}

int
main(int argc, char *argv[])
{
	char oper;
	uint32_t len;
	git_repository *repo = NULL;

	if (argc != 2) {
		fprintf(stderr, "Usage: elibgitd <repo path>\n");
		return 1;
	}

	if (git_repository_open(&repo, argv[1]) || repo == NULL) {
		fprintf(stderr, "Error opening repo\n");
		return 1;
	}

	// 0 = stdin
	// 1 = stdout
	while (1) {
		/* erlang writes the length first in packet mode. */
		if (read(0, &len, sizeof(uint32_t)) != sizeof(uint32_t))
			break;
		len = ntohl(len) - sizeof(char);
		if (read(0, &oper, sizeof(char)) != sizeof(char))
			break;
		switch (oper) {
		case OP_GETREF:
			handle_getref(repo, len);
			break;
		case OP_GETTREE:
			handle_gettree(repo, len);
			break;
		case OP_GETCOMMIT:
			handle_getcommit(repo, len);
			break;
		case OP_GETBLOB:
			handle_getblob(repo, len);
			break;
		case OP_BUILDTREE:
			handle_buildtree(repo, len);
			break;
		case OP_CREATEBLOB:
			handle_createblob(repo, len);
			break;
		case OP_CREATECOMMIT:
			handle_createcommit(repo, len);
			break;
		}
	}

	git_repository_free(repo);
	return 0;
}
