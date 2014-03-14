/* preamble {{{ */

struct mpd_settings;
struct mpd_connection;
struct mpd_song;
struct mpd_audio_format;
struct mpd_status;
struct mpd_stats;
struct mpd_song;
struct mpd_entity;
struct mpd_directory;
struct mpd_playlist;

struct mpd_pair {
	const char *name;
	const char *value;
};

enum mpd_server_error {
	MPD_SERVER_ERROR_UNK = -1,

	MPD_SERVER_ERROR_NOT_LIST = 1,
	MPD_SERVER_ERROR_ARG = 2,
	MPD_SERVER_ERROR_PASSWORD = 3,
	MPD_SERVER_ERROR_PERMISSION = 4,
	MPD_SERVER_ERROR_UNKNOWN_CMD = 5,

	MPD_SERVER_ERROR_NO_EXIST = 50,
	MPD_SERVER_ERROR_PLAYLIST_MAX = 51,
	MPD_SERVER_ERROR_SYSTEM = 52,
	MPD_SERVER_ERROR_PLAYLIST_LOAD = 53,
	MPD_SERVER_ERROR_UPDATE_ALREADY = 54,
	MPD_SERVER_ERROR_PLAYER_SYNC = 55,
	MPD_SERVER_ERROR_EXIST = 56
};

enum mpd_error {
	MPD_ERROR_SUCCESS = 0,
	MPD_ERROR_OOM,
	MPD_ERROR_ARGUMENT,
	MPD_ERROR_STATE,
	MPD_ERROR_TIMEOUT,
	MPD_ERROR_SYSTEM,
	MPD_ERROR_RESOLVER,
	MPD_ERROR_MALFORMED,
	MPD_ERROR_CLOSED,
	MPD_ERROR_SERVER
};

enum mpd_state {
	MPD_STATE_UNKNOWN = 0,
	MPD_STATE_STOP = 1,
	MPD_STATE_PLAY = 2,
	MPD_STATE_PAUSE = 3
};

enum mpd_tag_type
{
	MPD_TAG_UNKNOWN = -1,

	MPD_TAG_ARTIST,
	MPD_TAG_ALBUM,
	MPD_TAG_ALBUM_ARTIST,
	MPD_TAG_TITLE,
	MPD_TAG_TRACK,
	MPD_TAG_NAME,
	MPD_TAG_GENRE,
	MPD_TAG_DATE,
	MPD_TAG_COMPOSER,
	MPD_TAG_PERFORMER,
	MPD_TAG_COMMENT,
	MPD_TAG_DISC,

	MPD_TAG_MUSICBRAINZ_ARTISTID,
	MPD_TAG_MUSICBRAINZ_ALBUMID,
	MPD_TAG_MUSICBRAINZ_ALBUMARTISTID,
	MPD_TAG_MUSICBRAINZ_TRACKID,

	MPD_TAG_COUNT
};

enum mpd_entity_type {
	MPD_ENTITY_TYPE_UNKNOWN,
	MPD_ENTITY_TYPE_DIRECTORY,
	MPD_ENTITY_TYPE_SONG,
	MPD_ENTITY_TYPE_PLAYLIST
};

enum mpd_operator {
	MPD_OPERATOR_DEFAULT
};

/* preamble }}} */
/* <mpd/settings.h> {{{ */

struct mpd_settings *
mpd_settings_new(const char *host, unsigned int port, unsigned int timeout_ms,
		const char *reserved, const char *password);

void
mpd_settings_free(struct mpd_settings *settings);

const char *
mpd_settings_get_host(const struct mpd_settings *settings);

unsigned int
mpd_settings_get_port(const struct mpd_settings *settings);

unsigned int
mpd_settings_get_timeout_ms(const struct mpd_settings *settings);

const char *
mpd_settings_get_password(const struct mpd_settings *settings);

/* <mpd/settings.h> }}} */
/* <mpd/connection.h>  {{{ */

struct mpd_connection *
mpd_connection_new(const char *host, unsigned int port, unsigned int timeout_ms);

void
mpd_connection_free(struct mpd_connection *connection);

const struct mpd_settings *
mpd_connection_get_settings(const struct mpd_connection *connection);

enum mpd_error
mpd_connection_get_error(const struct mpd_connection *connection);

const char *
mpd_connection_get_error_message(const struct mpd_connection *connection);

enum mpd_server_error
mpd_connection_get_server_error(const struct mpd_connection *connection);

int
mpd_connection_get_system_error(const struct mpd_connection *connection);

bool
mpd_connection_clear_error(struct mpd_connection *connection);

/* <mpd/connection.h> }}} */
/* <mpd/player.h> {{{ */

struct mpd_song *
mpd_run_current_song(struct mpd_connection *connection);

bool
mpd_run_play(struct mpd_connection *connection);

bool
mpd_run_play_pos(struct mpd_connection *connection, unsigned int song_pos);

bool
mpd_run_play_id(struct mpd_connection *connection, unsigned int song_id);

bool
mpd_run_stop(struct mpd_connection *connection);

bool
mpd_run_toggle_pause(struct mpd_connection *connection);

bool
mpd_run_pause(struct mpd_connection *connection, bool mode);

bool
mpd_run_next(struct mpd_connection *connection);

bool
mpd_run_previous(struct mpd_connection *connection);

bool
mpd_run_seek_pos(struct mpd_connection *connection, unsigned int song_pos,
		unsigned int t);

bool
mpd_run_seek_id(struct mpd_connection *connection, unsigned int id,
		unsigned int t);

bool
mpd_run_repeat(struct mpd_connection *connection, bool mode);

bool
mpd_run_random(struct mpd_connection *connection, bool mode);

bool
mpd_run_single(struct mpd_connection *connection, bool mode);

bool
mpd_run_consume(struct mpd_connection *connection, bool mode);

bool
mpd_run_crossfade(struct mpd_connection *connection, unsigned int seconds);

bool
mpd_run_mixrampdb(struct mpd_connection *connection, float db);

bool
mpd_run_mixrampdelay(struct mpd_connection *connection, float seconds);

bool
mpd_run_clearerror(struct mpd_connection *connection);

/* <mpd/player.h> }}} */
/* <mpd/status.h> {{{ */

struct mpd_status *
mpd_run_status(struct mpd_connection *connection);

void
mpd_status_free(struct mpd_status *status);

int
mpd_status_get_volume(const struct mpd_status *status);

bool
mpd_status_get_repeat(const struct mpd_status *status);

bool
mpd_status_get_random(const struct mpd_status *status);

bool
mpd_status_get_single(const struct mpd_status *status);

bool
mpd_status_get_consume(const struct mpd_status *status);

unsigned int
mpd_status_get_queue_length(const struct mpd_status *status);

unsigned int
mpd_status_get_queue_version(const struct mpd_status *status);

enum mpd_state
mpd_status_get_state(const struct mpd_status *status);

unsigned int
mpd_status_get_crossfade(const struct mpd_status *status);

float
mpd_status_get_mixrampdb(const struct mpd_status *status);

float
mpd_status_get_mixrampdelay(const struct mpd_status *status);

int
mpd_status_get_song_pos(const struct mpd_status *status);

int
mpd_status_get_song_id(const struct mpd_status *status);

int
mpd_status_get_next_song_pos(const struct mpd_status *status);

int
mpd_status_get_next_song_id(const struct mpd_status *status);

unsigned int
mpd_status_get_elapsed_time(const struct mpd_status *status);

unsigned int
mpd_status_get_elapsed_ms(const struct mpd_status *status);

unsigned int
mpd_status_get_total_time(const struct mpd_status *status);

unsigned int
mpd_status_get_kbit_rate(const struct mpd_status *status);

const struct mpd_audio_format *
mpd_status_get_audio_format(const struct mpd_status *status);

unsigned int
mpd_status_get_update_id(const struct mpd_status *status);

const char *
mpd_status_get_error(const struct mpd_status *status);

/* <mpd/status.h> }}} */
/* <mpd/stats.h> {{{ */

struct mpd_stats *
mpd_run_stats(struct mpd_connection *connection);

void
mpd_stats_free(struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_artists(const struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_albums(const struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_songs(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_uptime(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_db_update_time(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_play_time(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_db_play_time(const struct mpd_stats *stats);

/* <mpd/stats.h> }}} */
/* <mpd/tag.h> {{{ */

const char *
mpd_tag_name(long type);

long
mpd_tag_name_parse(const char *name);

long
mpd_tag_name_iparse(const char *name);

/* <mpd/tag.h> }}} */
/* <mpd/song.h> {{{ */

void
mpd_song_free(struct mpd_song *song);

const char *
mpd_song_get_uri(const struct mpd_song *song);

const char *
mpd_song_get_tag(const struct mpd_song *song,
		long type, unsigned int idx);

unsigned int
mpd_song_get_duration(const struct mpd_song *song);

unsigned int
mpd_song_get_start(const struct mpd_song *song);

unsigned int
mpd_song_get_end(const struct mpd_song *song);

unsigned long
mpd_song_get_last_modified(const struct mpd_song *song);

void
mpd_song_set_pos(struct mpd_song *song, unsigned int pos);

unsigned int
mpd_song_get_pos(const struct mpd_song *song);

unsigned int
mpd_song_get_id(const struct mpd_song *song);

unsigned int
mpd_song_get_prio(const struct mpd_song *song);

struct mpd_song *
mpd_song_begin(const struct mpd_pair *pair);

bool
mpd_song_feed(struct mpd_song *song, const struct mpd_pair *pair);

struct mpd_song *
mpd_recv_song(struct mpd_connection *connection);

/* <mpd/song.h> }}} */
/* <mpd/queue.h> {{{ */

bool
mpd_send_list_queue_meta(struct mpd_connection *connection);

bool
mpd_send_list_queue_range_meta(struct mpd_connection *connection,
		unsigned int start, unsigned int end);

struct mpd_song *
mpd_run_get_queue_song_pos(struct mpd_connection *connection,
		unsigned int pos);

struct mpd_song *
mpd_run_get_queue_song_id(struct mpd_connection *connection, unsigned int id);

bool
mpd_send_queue_changes_meta(struct mpd_connection *connection,
		unsigned int version);

bool
mpd_run_add(struct mpd_connection *connection, const char *uri);

int
mpd_run_add_id(struct mpd_connection *connection, const char *file);

int
mpd_run_add_id_to(struct mpd_connection *connection, const char *uri,
		unsigned int to);

bool
mpd_run_delete(struct mpd_connection *connection, unsigned int pos);

bool
mpd_run_delete_range(struct mpd_connection *connection, unsigned int start,
		unsigned int end);

bool
mpd_run_delete_id(struct mpd_connection *connection, unsigned int id);

bool
mpd_run_shuffle(struct mpd_connection *connection);

bool
mpd_run_shuffle_range(struct mpd_connection *connection, unsigned int start,
		unsigned int end);

bool
mpd_run_clear(struct mpd_connection *connection);

bool
mpd_run_move(struct mpd_connection *connection, unsigned int from,
		unsigned int to);

bool
mpd_run_move_id(struct mpd_connection *connection, unsigned int from,
		unsigned int to);

bool
mpd_run_move_range(struct mpd_connection *connection, unsigned int start,
		unsigned int end, unsigned int to);

bool
mpd_run_swap(struct mpd_connection *connection, unsigned int pos1,
		unsigned int pos2);

bool
mpd_run_swap_id(struct mpd_connection *connection, unsigned int id1,
		unsigned int id2);

bool
mpd_run_prio(struct mpd_connection *connection, int priority,
		unsigned int position);

bool
mpd_run_prio_range(struct mpd_connection *connection, int priority,
		unsigned int start, unsigned int end);

bool
mpd_run_prio_id(struct mpd_connection *connection, int priority,
		unsigned int id);

/* <mpd/queue.h> }}} */
/* <mpd/search.h> {{{ */

bool
mpd_search_db_songs(struct mpd_connection *connection, bool exact);

bool
mpd_search_add_db_songs(struct mpd_connection *connection, bool exact);

bool
mpd_search_queue_songs(struct mpd_connection *connection, bool exact);

bool
mpd_search_db_tags(struct mpd_connection *connection, unsigned int type);

bool
mpd_count_db_songs(struct mpd_connection *connection);

bool
mpd_search_add_base_constraint(struct mpd_connection *connection,
		unsigned int oper, const char *value);

bool
mpd_search_add_uri_constraint(struct mpd_connection *connection,
		unsigned int oper, const char *value);

bool
mpd_search_add_tag_constraint(struct mpd_connection *connection,
		unsigned int oper, unsigned int type,
		const char *value);

bool
mpd_search_add_any_tag_constraint(struct mpd_connection *connection,
		unsigned int oper, const char *value);

bool
mpd_search_commit(struct mpd_connection *connection);

void
mpd_search_cancel(struct mpd_connection *connection);

struct mpd_pair *
mpd_recv_pair_tag(struct mpd_connection *connection, unsigned int type);

/* <mpd/search.h> }}} */
/* <mpd/recv.h> {{{ */

void
mpd_return_pair(struct mpd_connection *connection, struct mpd_pair *pair);

/* <mpd/recv.h> }}} */
/* <mpd/database.h> {{{ */

bool
mpd_send_list_all(struct mpd_connection *connection, const char *path);

bool
mpd_send_list_all_meta(struct mpd_connection *connection, const char *path);

bool
mpd_send_list_meta(struct mpd_connection *connection, const char *path);

bool
mpd_send_read_comments(struct mpd_connection *connection, const char *path);

unsigned int
mpd_run_update(struct mpd_connection *connection, const char *path);

unsigned int
mpd_run_rescan(struct mpd_connection *connection, const char *path);

/* <mpd/database.h> }}} */
/* <mpd/entity.h> {{{ */

void
mpd_entity_free(struct mpd_entity *entity);

enum mpd_entity_type
mpd_entity_get_type(const struct mpd_entity *entity);

const struct mpd_directory *
mpd_entity_get_directory(const struct mpd_entity *entity);

const struct mpd_song *
mpd_entity_get_song(const struct mpd_entity *entity);

const struct mpd_playlist *
mpd_entity_get_playlist(const struct mpd_entity *entity);

struct mpd_entity *
mpd_entity_begin(const struct mpd_pair *pair);

bool
mpd_entity_feed(struct mpd_entity *entity, const struct mpd_pair *pair);

struct mpd_entity *
mpd_recv_entity(struct mpd_connection *connection);

/* <mpd/entity.h> }}} */
/* <mpd/directory.h> {{{ */

struct mpd_directory *
mpd_directory_dup(const struct mpd_directory *directory);

void
mpd_directory_free(struct mpd_directory *directory);

const char *
mpd_directory_get_path(const struct mpd_directory *directory);

unsigned long
mpd_directory_get_last_modified(const struct mpd_directory *directory);

struct mpd_directory *
mpd_recv_directory(struct mpd_connection *connection);

/* <mpd/directory.h> }}} */
/* <mpd/playlist.h> {{{ */

void
mpd_playlist_free(struct mpd_playlist *playlist);

struct mpd_playlist *
mpd_playlist_dup(const struct mpd_playlist *playlist);

const char *
mpd_playlist_get_path(const struct mpd_playlist *playlist);

unsigned long
mpd_playlist_get_last_modified(const struct mpd_playlist *playlist);

bool
mpd_send_list_playlists(struct mpd_connection *connection);

struct mpd_playlist *
mpd_recv_playlist(struct mpd_connection *connection);

bool
mpd_send_list_playlist(struct mpd_connection *connection, const char *name);

bool
mpd_send_list_playlist_meta(struct mpd_connection *connection, const char *name);

bool
mpd_run_playlist_clear(struct mpd_connection *connection, const char *name);

bool
mpd_run_playlist_add(struct mpd_connection *connection,
		     const char *name, const char *path);

bool
mpd_send_playlist_move(struct mpd_connection *connection, const char *name,
		       unsigned int from, unsigned int to);

bool
mpd_run_playlist_delete(struct mpd_connection *connection,
			const char *name, unsigned int pos);

bool
mpd_run_save(struct mpd_connection *connection, const char *name);

bool
mpd_run_load(struct mpd_connection *connection, const char *name);

bool
mpd_run_rename(struct mpd_connection *connection,
	       const char *from, const char *to);

bool
mpd_run_rm(struct mpd_connection *connection, const char *name);

/* <mpd/playlist.h> }}} */
