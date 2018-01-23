;( function () {

'use strict';

hyogen.settings.print = function ( value ) {
  _( 'textarea' ).text( value );
};

var main = function ( code ) {
  new hyogen.Runtime()[ 'eval' ]( code );
};

_.fetch( 'code/import.hg' )
  .then( function ( res ) {
    return res.text();
  } )
  .then( main );

} )();
