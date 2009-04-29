/*
Copyright © 2009 Vivek Dasmohapatra 

email : vivek@etla.org
irc   : fledermaus on freenode, oftc
jabber: fledermaus@jabber.earth.li

This file is part of elim.

elim is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

elim is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with elim.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "blist_ui_ops.h"

static void _elim_bl_new_list         ( PurpleBuddyList *list );
static void _elim_bl_new_node         ( PurpleBlistNode *node );
static void _elim_bl_show             ( PurpleBuddyList *list );
static void _elim_bl_update           ( PurpleBuddyList *list ,
                                        PurpleBlistNode *node );
static void _elim_bl_remove           ( PurpleBuddyList *list ,
                                        PurpleBlistNode *node );
static void _elim_bl_destroy          ( PurpleBuddyList *list );
static void _elim_bl_set_visible      ( PurpleBuddyList *list , gboolean show );
static void _elim_bl_request_add_buddy( PurpleAccount *account  ,
                                        const char    *username ,
                                        const char    *group    ,
                                        const char    *alias    );
static void _elim_bl_request_add_chat ( PurpleAccount *account  ,
                                        PurpleGroup   *group    ,
                                        const char    *alias    ,
                                        const char    *name     );

#define PBLN_GET(thing,node) purple_blist_node_get_##thing( node )

#define UPDATE_RELATIVE( x, r_type, node, test )                              \
    {                                                                         \
        x = PBLN_GET( r_type, node );                                         \
        if( x && (test) )                                                     \
            add_outbound_sexp( __elim_bl_xnode(x, "elim-blist-update-node") );\
    }
// ==========================================================================

PurpleBlistUiOps elim_blist_ui_ops =
{
    _elim_bl_new_list          ,
    _elim_bl_new_node          ,
    _elim_bl_show              ,
    _elim_bl_update            ,
    _elim_bl_remove            ,
    _elim_bl_destroy           ,
    _elim_bl_set_visible       ,
    _elim_bl_request_add_buddy ,
    _elim_bl_request_add_chat
};

// ==========================================================================
static xmlnode * _elim_blnode_to_xnode( PurpleBlistNode *b );
static xmlnode * __elim_bl_xnode( PurpleBlistNode *node, const char *name );

static xmlnode * __elim_bl_xnode( PurpleBlistNode *node, const char *name )
{
    xmlnode *blnode = _elim_blnode_to_xnode( node );
    char    *ID     = new_elim_id   ();
    xmlnode *rval   = func_call( name, ID, blnode );
    g_free( ID );
    return rval;
}

static xmlnode * _elim_blnode_to_xnode( PurpleBlistNode *b )
{
    const char *bname   = NULL;
    const char *aname   = NULL;
    const char *proto   = NULL;
    const char *alias   = NULL;
    const char *s_alias = NULL;
    const char *c_alias = NULL;

    fprintf( stderr, "(_elim_blnode_to_xnode)\n" );

    int         type    = PBLN_GET( type, b );

    PurpleAccount    *acct = NULL;
    PurplePresence   *pres = NULL;
    PurpleStatus     *stat = NULL;
    PurpleContact *contact = NULL;

    switch( type )
    {
      case PURPLE_BLIST_BUDDY_NODE  :
        fprintf( stderr, "(_elim_blnode_to_xnode BUDDY)\n" );
        bname   = purple_buddy_get_name         ( (PurpleBuddy *)b );
        acct    = purple_buddy_get_account      ( (PurpleBuddy *)b );
        pres    = purple_buddy_get_presence     ( (PurpleBuddy *)b );
        alias   = purple_buddy_get_alias        ( (PurpleBuddy *)b );
        s_alias = purple_buddy_get_server_alias ( (PurpleBuddy *)b );
        c_alias = purple_buddy_get_contact_alias( (PurpleBuddy *)b );
        break;
      case PURPLE_BLIST_CHAT_NODE   :
        fprintf( stderr, "(_elim_blnode_to_xnode CHAT)\n" );
        bname = purple_chat_get_name   ( (PurpleChat *)b );
        acct  = purple_chat_get_account( (PurpleChat *)b );
        break;
      case PURPLE_BLIST_CONTACT_NODE:
        fprintf( stderr, "(_elim_blnode_to_xnode CONTACT)\n" );
        contact = (PurpleContact *)b;
        if( contact->alias ) { bname = alias = contact->alias; }
        else
        {
            PurpleBuddy     *buddy = purple_contact_get_priority_buddy(contact);
            PurpleBlistNode *child = NULL;
            if ( !buddy ) child = purple_blist_node_get_first_child( b );
            if( child )
                if ( PBLN_GET(type,child) == PURPLE_BLIST_BUDDY_NODE )
                    buddy = (PurpleBuddy *)child;
            if (  buddy ) bname = alias = purple_buddy_get_alias( buddy );
            else          bname = alias = "-";
        }
        break;
      case PURPLE_BLIST_GROUP_NODE  :
        fprintf( stderr, "(_elim_blnode_to_xnode GROUP)\n" );
        bname = purple_group_get_name( (PurpleGroup *)b );
        break;
      default:
        break;
    };

    if( acct )
    {
        aname = purple_account_get_username   ( acct );
        proto = purple_account_get_protocol_id( acct );
    }

    xmlnode *alist = xnode_new( "alist" );

    AL_PTR ( alist, "bnode-uid" , b );
    AL_ENUM( alist, "bnode-type", type, ":blist-node-type" );

    if( bname   ) AL_STR ( alist, "bnode-name"   , bname   );
    if( acct    ) AL_PTR ( alist, "account-uid"  , acct    );
    if( aname   ) AL_STR ( alist, "account-name" , aname   );
    if( proto   ) AL_STR ( alist, "im-protocol"  , proto   );
    if( alias   ) AL_STR ( alist, "bnode-alias"  , alias   );
    if( s_alias ) AL_STR ( alist, "server-alias" , s_alias );
    if( c_alias ) AL_STR ( alist, "contact-alias", c_alias );

    gpointer x = NULL;
    if((x = PBLN_GET(sibling_prev, b))) AL_PTR( alist, "bnode-prev"  , x );
    if((x = PBLN_GET(sibling_next, b))) AL_PTR( alist, "bnode-next"  , x );
    if((x = PBLN_GET(first_child , b))) AL_PTR( alist, "bnode-child" , x );
    if((x = PBLN_GET(parent      , b))) AL_PTR( alist, "bnode-parent", x );

    AL_ENUM( alist, "bnode-flags" , PBLN_GET( flags, b ), ":blist-node-flags" );

    if( contact && b )
    {
        PurpleContact *pc  = (PurpleContact *)b;
        PurpleBuddy   *pbn = purple_contact_get_priority_buddy( pc );
        AL_INT ( alist, "contact-size"            , pc->totalsize   );
        AL_INT ( alist, "contact-online-accounts" , pc->currentsize );
        AL_INT ( alist, "contact-online-buddies"  , pc->online      );
        AL_PTR ( alist, "contact-main-child-uid"  , pbn             );
    }

    if( acct && bname )
    {
        gboolean allowed = purple_privacy_check( acct, bname );
        AL_BOOL( alist, "allowed", allowed );
    }

    if( pres )
    {
        AL_BOOL( alist, "available", purple_presence_is_available( pres ) );
        AL_BOOL( alist, "online"   , purple_presence_is_online   ( pres ) );
        AL_BOOL( alist, "idle"     , purple_presence_is_idle     ( pres ) );
    }

    if( pres && ( stat = purple_presence_get_active_status( pres ) ) )
    {
        PurpleStatusType *stype = purple_status_get_type( stat );
        int              s_type = purple_status_type_get_primitive( stype );
        const char       *msg   = purple_status_get_attr_string(stat,"message");

        AL_STR ( alist, "status-name", purple_status_get_name ( stat ) );
        AL_STR ( alist, "status-id"  , purple_status_get_id   ( stat ) );
        AL_ENUM( alist, "status-type", s_type, ":status-primitive"     );
        AL_STR ( alist, "status-msg" , msg );
    }

    PurpleBlistNode *r = NULL;
    UPDATE_RELATIVE( r, sibling_prev, b, TRUE );
    UPDATE_RELATIVE( r, parent      , b, PBLN_GET( first_child, r ) == b );

    return alist;
}

// ==========================================================================

static void _elim_bl_new_list          ( PurpleBuddyList *list ) { }

static void _elim_bl_new_node( PurpleBlistNode *node )
{
    fprintf( stderr, "(_elim_bl_new_node)\n" );
    if( !PURPLE_BLIST_NODE_IS_CONTACT(node) )
        add_outbound_sexp( __elim_bl_xnode( node, "elim-blist-create-node" ) );
}

static void _elim_bl_update ( PurpleBuddyList *list , PurpleBlistNode *node )
{
    fprintf( stderr, "(_elim_bl_update)\n" );
    add_outbound_sexp( __elim_bl_xnode( node, "elim-blist-update-node" ) );
}

static void _elim_bl_remove ( PurpleBuddyList *list , PurpleBlistNode *node )
{
    fprintf( stderr, "(_elim_bl_remove)\n" );
    add_outbound_sexp( __elim_bl_xnode( node, "elim-blist-remove-node" ) );
}


static void _elim_bl_show         ( PurpleBuddyList *list ) {}
static void _elim_bl_destroy      ( PurpleBuddyList *list ) {}
static void _elim_bl_set_visible  ( PurpleBuddyList *list , gboolean show ) {}
static void _elim_bl_request_add_buddy ( PurpleAccount *account  ,
                                         const char    *username ,
                                         const char    *group    ,
                                         const char    *alias    ) {}
static void _elim_bl_request_add_chat  ( PurpleAccount *account  ,
                                         PurpleGroup   *group    ,
                                         const char    *alias    ,
                                         const char    *name     ) {}
