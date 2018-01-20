;( function ( window, undefined ) {

'use strict';

var main = function ( code ) {
  new hyogen.Runtime()[ 'eval' ]( code );
};

_.fetch( 'code/import.hg' )
  .then( function ( res ) {
    return res.text();
  } )
  .then( main );

} )( this );
