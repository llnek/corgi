// Compiled by ClojureScript 1.10.238 {}
goog.provide('czlab.elmo.afx.ecs');
goog.require('cljs.core');
goog.require('oops.core');
/**
 * 
 */
czlab.elmo.afx.ecs.raise_BANG_ = (function czlab$elmo$afx$ecs$raise_BANG_(var_args){
var args__4502__auto__ = [];
var len__4499__auto___2904 = arguments.length;
var i__4500__auto___2905 = (0);
while(true){
if((i__4500__auto___2905 < len__4499__auto___2904)){
args__4502__auto__.push((arguments[i__4500__auto___2905]));

var G__2906 = (i__4500__auto___2905 + (1));
i__4500__auto___2905 = G__2906;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((0) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((0)),(0),null)):null);
return czlab.elmo.afx.ecs.raise_BANG_.cljs$core$IFn$_invoke$arity$variadic(argseq__4503__auto__);
});

czlab.elmo.afx.ecs.raise_BANG_.cljs$core$IFn$_invoke$arity$variadic = (function (args){
throw (new Error(cljs.core.apply.call(null,cljs.core.str,args)));
});

czlab.elmo.afx.ecs.raise_BANG_.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
czlab.elmo.afx.ecs.raise_BANG_.cljs$lang$applyTo = (function (seq2903){
var self__4487__auto__ = this;
return self__4487__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq2903));
});

/**
 * 
 */
czlab.elmo.afx.ecs.createPool = (function czlab$elmo$afx$ecs$createPool(var_args){
var args__4502__auto__ = [];
var len__4499__auto___2918 = arguments.length;
var i__4500__auto___2919 = (0);
while(true){
if((i__4500__auto___2919 < len__4499__auto___2918)){
args__4502__auto__.push((arguments[i__4500__auto___2919]));

var G__2920 = (i__4500__auto___2919 + (1));
i__4500__auto___2919 = G__2920;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.createPool.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.createPool.cljs$core$IFn$_invoke$arity$variadic = (function (ctor,rinse,p__2911){
var vec__2912 = p__2911;
var batch = cljs.core.nth.call(null,vec__2912,(0),null);
if(cljs.core.fn_QMARK_.call(null,ctor)){
} else {
throw (new Error("Assert failed: (fn? ctor)"));
}

if(cljs.core.fn_QMARK_.call(null,rinse)){
} else {
throw (new Error("Assert failed: (fn? rinse)"));
}

var a = cljs.core.atom.call(null,new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"batch","batch",-662921200),(function (){var or__3922__auto__ = batch;
if(cljs.core.truth_(or__3922__auto__)){
return or__3922__auto__;
} else {
return (10);
}
})(),new cljs.core.Keyword(null,"size","size",1098693007),(0),new cljs.core.Keyword(null,"next","next",-117701485),(0),new cljs.core.Keyword(null,"slots","slots",276818598),[],new cljs.core.Keyword(null,"ctor","ctor",1750864802),ctor,new cljs.core.Keyword(null,"rinse","rinse",-1720262066),rinse], null));
var g = ((function (a,vec__2912,batch){
return (function (b){
var n__4376__auto___2921 = new cljs.core.Keyword(null,"batch","batch",-662921200).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,a));
var __2922 = (0);
while(true){
if((__2922 < n__4376__auto___2921)){
b.push((function (){var target_obj_2915 = ctor.call(null);
var _STAR_runtime_state_STAR_2917 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2915,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2916_2923 = target_obj_2915;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2916_2923,(0),"____pool",true,true,true)){
(parent_obj_2916_2923["____pool"] = a);
} else {
}

return target_obj_2915;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2917;
}})());

var G__2924 = (__2922 + (1));
__2922 = G__2924;
continue;
} else {
}
break;
}

return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(new cljs.core.Keyword(null,"size","size",1098693007).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,a)) + new cljs.core.Keyword(null,"batch","batch",-662921200).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,a))),b], null);
});})(a,vec__2912,batch))
;
cljs.core.swap_BANG_.call(null,a,((function (a,g,vec__2912,batch){
return (function (p1__2907_SHARP_){
return cljs.core.merge.call(null,p1__2907_SHARP_,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"grow","grow",-524118895),g], null));
});})(a,g,vec__2912,batch))
);

return a;
});

czlab.elmo.afx.ecs.createPool.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.createPool.cljs$lang$applyTo = (function (seq2908){
var G__2909 = cljs.core.first.call(null,seq2908);
var seq2908__$1 = cljs.core.next.call(null,seq2908);
var G__2910 = cljs.core.first.call(null,seq2908__$1);
var seq2908__$2 = cljs.core.next.call(null,seq2908__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__2909,G__2910,seq2908__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.countUsedInPool = (function czlab$elmo$afx$ecs$countUsedInPool(pool){
return new cljs.core.Keyword(null,"next","next",-117701485).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,pool));
});
/**
 * 
 */
czlab.elmo.afx.ecs.sizeOfPool = (function czlab$elmo$afx$ecs$sizeOfPool(pool){
return new cljs.core.Keyword(null,"size","size",1098693007).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,pool));
});
/**
 * 
 */
czlab.elmo.afx.ecs.takeFromPool_BANG_ = (function czlab$elmo$afx$ecs$takeFromPool_BANG_(pool){
var out = cljs.core.atom.call(null,null);
cljs.core.swap_BANG_.call(null,pool,((function (out){
return (function (p__2925){
var map__2926 = p__2925;
var map__2926__$1 = ((((!((map__2926 == null)))?(((((map__2926.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__2926.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__2926):map__2926);
var root = map__2926__$1;
var grow = cljs.core.get.call(null,map__2926__$1,new cljs.core.Keyword(null,"grow","grow",-524118895));
var size = cljs.core.get.call(null,map__2926__$1,new cljs.core.Keyword(null,"size","size",1098693007));
var next = cljs.core.get.call(null,map__2926__$1,new cljs.core.Keyword(null,"next","next",-117701485));
var slots = cljs.core.get.call(null,map__2926__$1,new cljs.core.Keyword(null,"slots","slots",276818598));
var next1 = (next + (1));
var vec__2928 = (((next >= size))?grow.call(null,slots):new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [size,slots], null));
var sz = cljs.core.nth.call(null,vec__2928,(0),null);
var buf = cljs.core.nth.call(null,vec__2928,(1),null);
cljs.core.reset_BANG_.call(null,out,(buf[next]));

var G__2931_2938 = cljs.core.deref.call(null,out);
var target_obj_2932_2939 = G__2931_2938;
var _STAR_runtime_state_STAR_2934_2940 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2932_2939,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2933_2941 = target_obj_2932_2939;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2933_2941,(0),"____slot",true,true,true)){
(parent_obj_2933_2941["____slot"] = next);
} else {
}

}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2934_2940;
}
var target_obj_2935_2942 = G__2931_2938;
var _STAR_runtime_state_STAR_2937_2943 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2935_2942,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2936_2944 = target_obj_2935_2942;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2936_2944,(0),"____status",true,true,true)){
(parent_obj_2936_2944["____status"] = true);
} else {
}

}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2937_2943;
}

return cljs.core.merge.call(null,root,((cljs.core._EQ_.call(null,sz,size))?new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"next","next",-117701485),next1], null):new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"next","next",-117701485),next1,new cljs.core.Keyword(null,"size","size",1098693007),sz], null)));
});})(out))
);

return cljs.core.deref.call(null,out);
});
/**
 * 
 */
czlab.elmo.afx.ecs.returnToPool_BANG_ = (function czlab$elmo$afx$ecs$returnToPool_BANG_(pool,obj){
if(cljs.core.truth_((function (){var and__3911__auto__ = !((obj == null));
if(and__3911__auto__){
var and__3911__auto____$1 = (function (){var target_obj_2951 = obj;
var _STAR_runtime_state_STAR_2953 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2951,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var next_obj_2952 = ((oops.core.validate_object_access_dynamically.call(null,target_obj_2951,(0),"____status",true,true,false))?(target_obj_2951["____status"]):null);
return next_obj_2952;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2953;
}})();
if(cljs.core.truth_(and__3911__auto____$1)){
return ((function (){var target_obj_2954 = obj;
var _STAR_runtime_state_STAR_2956 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2954,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var next_obj_2955 = ((oops.core.validate_object_access_dynamically.call(null,target_obj_2954,(0),"____pool",true,true,false))?(target_obj_2954["____pool"]):null);
return next_obj_2955;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2956;
}})() === pool);
} else {
return and__3911__auto____$1;
}
} else {
return and__3911__auto__;
}
})())){
cljs.core.swap_BANG_.call(null,pool,(function (p__2957){
var map__2958 = p__2957;
var map__2958__$1 = ((((!((map__2958 == null)))?(((((map__2958.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__2958.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__2958):map__2958);
var root = map__2958__$1;
var rinse = cljs.core.get.call(null,map__2958__$1,new cljs.core.Keyword(null,"rinse","rinse",-1720262066));
var next = cljs.core.get.call(null,map__2958__$1,new cljs.core.Keyword(null,"next","next",-117701485));
var slots = cljs.core.get.call(null,map__2958__$1,new cljs.core.Keyword(null,"slots","slots",276818598));
var next1 = (next - (1));
var _ = rinse.call(null,obj);
var tail = (slots[next1]);
var slot_SINGLEQUOTE_ = (function (){var target_obj_2960 = tail;
var _STAR_runtime_state_STAR_2962 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2960,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var next_obj_2961 = ((oops.core.validate_object_access_dynamically.call(null,target_obj_2960,(0),"____slot",true,true,false))?(target_obj_2960["____slot"]):null);
return next_obj_2961;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2962;
}})();
var epos_SINGLEQUOTE_ = (function (){var target_obj_2963 = obj;
var _STAR_runtime_state_STAR_2965 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2963,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var next_obj_2964 = ((oops.core.validate_object_access_dynamically.call(null,target_obj_2963,(0),"____slot",true,true,false))?(target_obj_2963["____slot"]):null);
return next_obj_2964;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2965;
}})();
(slots[next1] = obj);

(slots[epos_SINGLEQUOTE_] = tail);

var target_obj_2966_2975 = tail;
var _STAR_runtime_state_STAR_2968_2976 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2966_2975,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2967_2977 = target_obj_2966_2975;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2967_2977,(0),"____slot",true,true,true)){
(parent_obj_2967_2977["____slot"] = epos_SINGLEQUOTE_);
} else {
}

}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2968_2976;
}
var target_obj_2969_2978 = obj;
var _STAR_runtime_state_STAR_2971_2979 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2969_2978,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2970_2980 = target_obj_2969_2978;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2970_2980,(0),"____slot",true,true,true)){
(parent_obj_2970_2980["____slot"] = slot_SINGLEQUOTE_);
} else {
}

}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2971_2979;
}
var target_obj_2972_2981 = obj;
var _STAR_runtime_state_STAR_2974_2982 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2972_2981,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_2973_2983 = target_obj_2972_2981;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_2973_2983,(0),"____status",true,true,true)){
(parent_obj_2973_2983["____status"] = false);
} else {
}

}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2974_2982;
}
return cljs.core.merge.call(null,root,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"next","next",-117701485),next1], null));
}));
} else {
}

return pool;
});
/**
 * 
 */
czlab.elmo.afx.ecs.createECS = (function czlab$elmo$afx$ecs$createECS(){
return cljs.core.atom.call(null,new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"entities","entities",1940967403),cljs.core.PersistentHashSet.EMPTY,new cljs.core.Keyword(null,"templates","templates",-1237401733),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"registry","registry",1021159018),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"data","data",-232669377),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"systems","systems",-1015374944),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"uid","uid",-1447769400),(1)], null));
});
/**
 * 
 */
czlab.elmo.afx.ecs.genUid = (function czlab$elmo$afx$ecs$genUid(ecs){
var out = cljs.core.atom.call(null,(0));
cljs.core.swap_BANG_.call(null,ecs,((function (out){
return (function (p__2984){
var map__2985 = p__2984;
var map__2985__$1 = ((((!((map__2985 == null)))?(((((map__2985.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__2985.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__2985):map__2985);
var root = map__2985__$1;
var uid = cljs.core.get.call(null,map__2985__$1,new cljs.core.Keyword(null,"uid","uid",-1447769400));
cljs.core.reset_BANG_.call(null,out,uid);

return cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"uid","uid",-1447769400),(uid + (1)));
});})(out))
);

return cljs.core.deref.call(null,out);
});
/**
 * 
 */
czlab.elmo.afx.ecs.retUsed = (function czlab$elmo$afx$ecs$retUsed(obj){
if(cljs.core.vector_QMARK_.call(null,obj)){
var seq__2987 = cljs.core.seq.call(null,obj);
var chunk__2988 = null;
var count__2989 = (0);
var i__2990 = (0);
while(true){
if((i__2990 < count__2989)){
var c = cljs.core._nth.call(null,chunk__2988,i__2990);
czlab.elmo.afx.ecs.retUsed.call(null,c);


var G__2994 = seq__2987;
var G__2995 = chunk__2988;
var G__2996 = count__2989;
var G__2997 = (i__2990 + (1));
seq__2987 = G__2994;
chunk__2988 = G__2995;
count__2989 = G__2996;
i__2990 = G__2997;
continue;
} else {
var temp__5457__auto__ = cljs.core.seq.call(null,seq__2987);
if(temp__5457__auto__){
var seq__2987__$1 = temp__5457__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__2987__$1)){
var c__4319__auto__ = cljs.core.chunk_first.call(null,seq__2987__$1);
var G__2998 = cljs.core.chunk_rest.call(null,seq__2987__$1);
var G__2999 = c__4319__auto__;
var G__3000 = cljs.core.count.call(null,c__4319__auto__);
var G__3001 = (0);
seq__2987 = G__2998;
chunk__2988 = G__2999;
count__2989 = G__3000;
i__2990 = G__3001;
continue;
} else {
var c = cljs.core.first.call(null,seq__2987__$1);
czlab.elmo.afx.ecs.retUsed.call(null,c);


var G__3002 = cljs.core.next.call(null,seq__2987__$1);
var G__3003 = null;
var G__3004 = (0);
var G__3005 = (0);
seq__2987 = G__3002;
chunk__2988 = G__3003;
count__2989 = G__3004;
i__2990 = G__3005;
continue;
}
} else {
return null;
}
}
break;
}
} else {
if(cljs.core.map_QMARK_.call(null,obj)){
return czlab.elmo.afx.ecs.retUsed.call(null,cljs.core.vals.call(null,obj));
} else {
if(cljs.core.object_QMARK_.call(null,obj)){
var temp__5459__auto__ = (function (){var target_obj_2991 = obj;
var _STAR_runtime_state_STAR_2993 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_2991,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var next_obj_2992 = ((oops.core.validate_object_access_dynamically.call(null,target_obj_2991,(0),"____pool",true,true,false))?(target_obj_2991["____pool"]):null);
return next_obj_2992;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_2993;
}})();
if((temp__5459__auto__ == null)){
return null;
} else {
var p = temp__5459__auto__;
return czlab.elmo.afx.ecs.returnToPool_BANG_.call(null,p,obj);
}
} else {
return null;
}
}
}
});
/**
 * 
 */
czlab.elmo.afx.ecs.remEnt = (function czlab$elmo$afx$ecs$remEnt(p__3006,ents){
var map__3007 = p__3006;
var map__3007__$1 = ((((!((map__3007 == null)))?(((((map__3007.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__3007.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__3007):map__3007);
var root = map__3007__$1;
var data = cljs.core.get.call(null,map__3007__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var registry = cljs.core.get.call(null,map__3007__$1,new cljs.core.Keyword(null,"registry","registry",1021159018));
var dt = (function (){var dt = data;
var G__3012 = cljs.core.keys.call(null,registry);
var vec__3013 = G__3012;
var seq__3014 = cljs.core.seq.call(null,vec__3013);
var first__3015 = cljs.core.first.call(null,seq__3014);
var seq__3014__$1 = cljs.core.next.call(null,seq__3014);
var k = first__3015;
var xs = seq__3014__$1;
var dt__$1 = dt;
var G__3012__$1 = G__3012;
while(true){
var dt__$2 = dt__$1;
var vec__3016 = G__3012__$1;
var seq__3017 = cljs.core.seq.call(null,vec__3016);
var first__3018 = cljs.core.first.call(null,seq__3017);
var seq__3017__$1 = cljs.core.next.call(null,seq__3017);
var k__$1 = first__3018;
var xs__$1 = seq__3017__$1;
if(!(!((k__$1 == null)))){
return dt__$2;
} else {
var cOrig = cljs.core.get.call(null,dt__$2,k__$1);
var ct = (function (){var ct = cOrig;
var G__3022 = ents;
var vec__3023 = G__3022;
var seq__3024 = cljs.core.seq.call(null,vec__3023);
var first__3025 = cljs.core.first.call(null,seq__3024);
var seq__3024__$1 = cljs.core.next.call(null,seq__3024);
var e = first__3025;
var xs__$2 = seq__3024__$1;
var ct__$1 = ct;
var G__3022__$1 = G__3022;
while(true){
var ct__$2 = ct__$1;
var vec__3026 = G__3022__$1;
var seq__3027 = cljs.core.seq.call(null,vec__3026);
var first__3028 = cljs.core.first.call(null,seq__3027);
var seq__3027__$1 = cljs.core.next.call(null,seq__3027);
var e__$1 = first__3028;
var xs__$3 = seq__3027__$1;
if(!(!((e__$1 == null)))){
return ct__$2;
} else {
var G__3029 = (function (){var temp__5459__auto__ = cljs.core.get.call(null,ct__$2,e__$1);
if((temp__5459__auto__ == null)){
return ct__$2;
} else {
var v = temp__5459__auto__;
czlab.elmo.afx.ecs.retUsed.call(null,v);

return cljs.core.dissoc.call(null,ct__$2,e__$1);
}
})();
var G__3030 = xs__$3;
ct__$1 = G__3029;
G__3022__$1 = G__3030;
continue;
}
break;
}
})();
var G__3031 = (((ct === cOrig))?dt__$2:cljs.core.assoc.call(null,dt__$2,k__$1,ct));
var G__3032 = xs__$1;
dt__$1 = G__3031;
G__3012__$1 = G__3032;
continue;
}
break;
}
})();
var rt = (((dt === data))?root:cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"data","data",-232669377),dt));
return cljs.core.assoc.call(null,rt,new cljs.core.Keyword(null,"entities","entities",1940967403),cljs.core.apply.call(null,cljs.core.disj,new cljs.core.Keyword(null,"entities","entities",1940967403).cljs$core$IFn$_invoke$arity$1(rt),ents));
});
/**
 * 
 */
czlab.elmo.afx.ecs.removeEntity = (function czlab$elmo$afx$ecs$removeEntity(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3037 = arguments.length;
var i__4500__auto___3038 = (0);
while(true){
if((i__4500__auto___3038 < len__4499__auto___3037)){
args__4502__auto__.push((arguments[i__4500__auto___3038]));

var G__3039 = (i__4500__auto___3038 + (1));
i__4500__auto___3038 = G__3039;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.removeEntity.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.removeEntity.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,entity,more){
cljs.core.swap_BANG_.call(null,ecs,(function (p1__3033_SHARP_){
return czlab.elmo.afx.ecs.remEnt.call(null,p1__3033_SHARP_,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [entity], null),more));
}));

return ecs;
});

czlab.elmo.afx.ecs.removeEntity.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.removeEntity.cljs$lang$applyTo = (function (seq3034){
var G__3035 = cljs.core.first.call(null,seq3034);
var seq3034__$1 = cljs.core.next.call(null,seq3034);
var G__3036 = cljs.core.first.call(null,seq3034__$1);
var seq3034__$2 = cljs.core.next.call(null,seq3034__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3035,G__3036,seq3034__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.addComponent = (function czlab$elmo$afx$ecs$addComponent(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3045 = arguments.length;
var i__4500__auto___3046 = (0);
while(true){
if((i__4500__auto___3046 < len__4499__auto___3045)){
args__4502__auto__.push((arguments[i__4500__auto___3046]));

var G__3047 = (i__4500__auto___3046 + (1));
i__4500__auto___3046 = G__3047;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((3) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((3)),(0),null)):null);
return czlab.elmo.afx.ecs.addComponent.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.addComponent.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,id,component,more){
cljs.core.swap_BANG_.call(null,ecs,(function (p1__3040_SHARP_){
return cljs.core.assoc.call(null,p1__3040_SHARP_,new cljs.core.Keyword(null,"registry","registry",1021159018),cljs.core.merge.call(null,new cljs.core.Keyword(null,"registry","registry",1021159018).cljs$core$IFn$_invoke$arity$1(p1__3040_SHARP_),cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,cljs.core.partition.call(null,(2),cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [id,component], null),more)))));
}));

return ecs;
});

czlab.elmo.afx.ecs.addComponent.cljs$lang$maxFixedArity = (3);

/** @this {Function} */
czlab.elmo.afx.ecs.addComponent.cljs$lang$applyTo = (function (seq3041){
var G__3042 = cljs.core.first.call(null,seq3041);
var seq3041__$1 = cljs.core.next.call(null,seq3041);
var G__3043 = cljs.core.first.call(null,seq3041__$1);
var seq3041__$2 = cljs.core.next.call(null,seq3041__$1);
var G__3044 = cljs.core.first.call(null,seq3041__$2);
var seq3041__$3 = cljs.core.next.call(null,seq3041__$2);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3042,G__3043,G__3044,seq3041__$3);
});

/**
 * 
 */
czlab.elmo.afx.ecs.removeComponent = (function czlab$elmo$afx$ecs$removeComponent(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3062 = arguments.length;
var i__4500__auto___3063 = (0);
while(true){
if((i__4500__auto___3063 < len__4499__auto___3062)){
args__4502__auto__.push((arguments[i__4500__auto___3063]));

var G__3064 = (i__4500__auto___3063 + (1));
i__4500__auto___3063 = G__3064;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.removeComponent.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.removeComponent.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,id,more){
cljs.core.swap_BANG_.call(null,ecs,(function (p__3051){
var map__3052 = p__3051;
var map__3052__$1 = ((((!((map__3052 == null)))?(((((map__3052.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__3052.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__3052):map__3052);
var root = map__3052__$1;
var data = cljs.core.get.call(null,map__3052__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var registry = cljs.core.get.call(null,map__3052__$1,new cljs.core.Keyword(null,"registry","registry",1021159018));
var cids = cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [id], null),more);
var seq__3054_3065 = cljs.core.seq.call(null,cids);
var chunk__3057_3066 = null;
var count__3058_3067 = (0);
var i__3059_3068 = (0);
while(true){
if((i__3059_3068 < count__3058_3067)){
var c_3069 = cljs.core._nth.call(null,chunk__3057_3066,i__3059_3068);
var v_3070 = cljs.core.get.call(null,data,c_3069);
if(!((v_3070 == null))){
czlab.elmo.afx.ecs.retUsed.call(null,v_3070);


var G__3071 = seq__3054_3065;
var G__3072 = chunk__3057_3066;
var G__3073 = count__3058_3067;
var G__3074 = (i__3059_3068 + (1));
seq__3054_3065 = G__3071;
chunk__3057_3066 = G__3072;
count__3058_3067 = G__3073;
i__3059_3068 = G__3074;
continue;
} else {
var G__3075 = seq__3054_3065;
var G__3076 = chunk__3057_3066;
var G__3077 = count__3058_3067;
var G__3078 = (i__3059_3068 + (1));
seq__3054_3065 = G__3075;
chunk__3057_3066 = G__3076;
count__3058_3067 = G__3077;
i__3059_3068 = G__3078;
continue;
}
} else {
var temp__5457__auto___3079 = cljs.core.seq.call(null,seq__3054_3065);
if(temp__5457__auto___3079){
var seq__3054_3080__$1 = temp__5457__auto___3079;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__3054_3080__$1)){
var c__4319__auto___3081 = cljs.core.chunk_first.call(null,seq__3054_3080__$1);
var G__3082 = cljs.core.chunk_rest.call(null,seq__3054_3080__$1);
var G__3083 = c__4319__auto___3081;
var G__3084 = cljs.core.count.call(null,c__4319__auto___3081);
var G__3085 = (0);
seq__3054_3065 = G__3082;
chunk__3057_3066 = G__3083;
count__3058_3067 = G__3084;
i__3059_3068 = G__3085;
continue;
} else {
var c_3086 = cljs.core.first.call(null,seq__3054_3080__$1);
var v_3087 = cljs.core.get.call(null,data,c_3086);
if(!((v_3087 == null))){
czlab.elmo.afx.ecs.retUsed.call(null,v_3087);


var G__3088 = cljs.core.next.call(null,seq__3054_3080__$1);
var G__3089 = null;
var G__3090 = (0);
var G__3091 = (0);
seq__3054_3065 = G__3088;
chunk__3057_3066 = G__3089;
count__3058_3067 = G__3090;
i__3059_3068 = G__3091;
continue;
} else {
var G__3092 = cljs.core.next.call(null,seq__3054_3080__$1);
var G__3093 = null;
var G__3094 = (0);
var G__3095 = (0);
seq__3054_3065 = G__3092;
chunk__3057_3066 = G__3093;
count__3058_3067 = G__3094;
i__3059_3068 = G__3095;
continue;
}
}
} else {
}
}
break;
}

return cljs.core.assoc.call(null,cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"data","data",-232669377),cljs.core.apply.call(null,cljs.core.dissoc,data,cids)),new cljs.core.Keyword(null,"registry","registry",1021159018),cljs.core.apply.call(null,cljs.core.dissoc,registry,cids));
}));

return ecs;
});

czlab.elmo.afx.ecs.removeComponent.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.removeComponent.cljs$lang$applyTo = (function (seq3048){
var G__3049 = cljs.core.first.call(null,seq3048);
var seq3048__$1 = cljs.core.next.call(null,seq3048);
var G__3050 = cljs.core.first.call(null,seq3048__$1);
var seq3048__$2 = cljs.core.next.call(null,seq3048__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3049,G__3050,seq3048__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.addToEntity = (function czlab$elmo$afx$ecs$addToEntity(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3120 = arguments.length;
var i__4500__auto___3121 = (0);
while(true){
if((i__4500__auto___3121 < len__4499__auto___3120)){
args__4502__auto__.push((arguments[i__4500__auto___3121]));

var G__3122 = (i__4500__auto___3121 + (1));
i__4500__auto___3121 = G__3122;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((3) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((3)),(0),null)):null);
return czlab.elmo.afx.ecs.addToEntity.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.addToEntity.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,entity,componentDecl,moreDecls){
cljs.core.swap_BANG_.call(null,ecs,(function (p__3101){
var map__3102 = p__3101;
var map__3102__$1 = ((((!((map__3102 == null)))?(((((map__3102.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__3102.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__3102):map__3102);
var root = map__3102__$1;
var data = cljs.core.get.call(null,map__3102__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var registry = cljs.core.get.call(null,map__3102__$1,new cljs.core.Keyword(null,"registry","registry",1021159018));
return cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"data","data",-232669377),(function (){var dtree = data;
var G__3107 = cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [componentDecl], null),moreDecls);
var vec__3108 = G__3107;
var seq__3109 = cljs.core.seq.call(null,vec__3108);
var first__3110 = cljs.core.first.call(null,seq__3109);
var seq__3109__$1 = cljs.core.next.call(null,seq__3109);
var dc = first__3110;
var xs = seq__3109__$1;
var dtree__$1 = dtree;
var G__3107__$1 = G__3107;
while(true){
var dtree__$2 = dtree__$1;
var vec__3111 = G__3107__$1;
var seq__3112 = cljs.core.seq.call(null,vec__3111);
var first__3113 = cljs.core.first.call(null,seq__3112);
var seq__3112__$1 = cljs.core.next.call(null,seq__3112);
var dc__$1 = first__3113;
var xs__$1 = seq__3112__$1;
if(!(!((dc__$1 == null)))){
return dtree__$2;
} else {
var vec__3114 = dc__$1;
var seq__3115 = cljs.core.seq.call(null,vec__3114);
var first__3116 = cljs.core.first.call(null,seq__3115);
var seq__3115__$1 = cljs.core.next.call(null,seq__3115);
var cid = first__3116;
var args = seq__3115__$1;
var ctor = cljs.core.get.call(null,registry,cid);
var _ = ((!(cljs.core.fn_QMARK_.call(null,ctor)))?czlab.elmo.afx.ecs.raise_BANG_.call(null,"Unknown component ",cid):null);
var co = (function (){var target_obj_3117 = cljs.core.apply.call(null,ctor,args);
var _STAR_runtime_state_STAR_3119 = oops.state._STAR_runtime_state_STAR_;
oops.state._STAR_runtime_state_STAR_ = oops.state.prepare_state.call(null,target_obj_3117,(new Error()),function(){arguments[0].apply(console,Array.prototype.slice.call(arguments,1))});

try{var parent_obj_3118_3123 = target_obj_3117;
if(oops.core.validate_object_access_dynamically.call(null,parent_obj_3118_3123,(0),"____entity",true,true,true)){
(parent_obj_3118_3123["____entity"] = entity);
} else {
}

return target_obj_3117;
}finally {oops.state._STAR_runtime_state_STAR_ = _STAR_runtime_state_STAR_3119;
}})();
var G__3124 = cljs.core.update_in.call(null,dtree__$2,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cid], null),((function (dtree__$1,G__3107__$1,vec__3114,seq__3115,first__3116,seq__3115__$1,cid,args,ctor,_,co,dtree__$2,vec__3111,seq__3112,first__3113,seq__3112__$1,dc__$1,xs__$1,dtree,G__3107,vec__3108,seq__3109,first__3110,seq__3109__$1,dc,xs,map__3102,map__3102__$1,root,data,registry){
return (function (p1__3096_SHARP_){
return cljs.core.assoc.call(null,p1__3096_SHARP_,entity,co);
});})(dtree__$1,G__3107__$1,vec__3114,seq__3115,first__3116,seq__3115__$1,cid,args,ctor,_,co,dtree__$2,vec__3111,seq__3112,first__3113,seq__3112__$1,dc__$1,xs__$1,dtree,G__3107,vec__3108,seq__3109,first__3110,seq__3109__$1,dc,xs,map__3102,map__3102__$1,root,data,registry))
);
var G__3125 = xs__$1;
dtree__$1 = G__3124;
G__3107__$1 = G__3125;
continue;
}
break;
}
})());
}));

return ecs;
});

czlab.elmo.afx.ecs.addToEntity.cljs$lang$maxFixedArity = (3);

/** @this {Function} */
czlab.elmo.afx.ecs.addToEntity.cljs$lang$applyTo = (function (seq3097){
var G__3098 = cljs.core.first.call(null,seq3097);
var seq3097__$1 = cljs.core.next.call(null,seq3097);
var G__3099 = cljs.core.first.call(null,seq3097__$1);
var seq3097__$2 = cljs.core.next.call(null,seq3097__$1);
var G__3100 = cljs.core.first.call(null,seq3097__$2);
var seq3097__$3 = cljs.core.next.call(null,seq3097__$2);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3098,G__3099,G__3100,seq3097__$3);
});

/**
 * 
 */
czlab.elmo.afx.ecs.createEntity = (function czlab$elmo$afx$ecs$createEntity(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3130 = arguments.length;
var i__4500__auto___3131 = (0);
while(true){
if((i__4500__auto___3131 < len__4499__auto___3130)){
args__4502__auto__.push((arguments[i__4500__auto___3131]));

var G__3132 = (i__4500__auto___3131 + (1));
i__4500__auto___3131 = G__3132;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.createEntity.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.createEntity.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,componentDecl,moreDecls){
var entity = czlab.elmo.afx.ecs.genUid.call(null,ecs);
cljs.core.apply.call(null,czlab.elmo.afx.ecs.addToEntity,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [ecs,entity,componentDecl], null),moreDecls));

cljs.core.swap_BANG_.call(null,ecs,((function (entity){
return (function (p1__3126_SHARP_){
return cljs.core.update_in.call(null,p1__3126_SHARP_,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"entities","entities",1940967403)], null),((function (entity){
return (function (c){
return cljs.core.conj.call(null,c,entity);
});})(entity))
);
});})(entity))
);

return entity;
});

czlab.elmo.afx.ecs.createEntity.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.createEntity.cljs$lang$applyTo = (function (seq3127){
var G__3128 = cljs.core.first.call(null,seq3127);
var seq3127__$1 = cljs.core.next.call(null,seq3127);
var G__3129 = cljs.core.first.call(null,seq3127__$1);
var seq3127__$2 = cljs.core.next.call(null,seq3127__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3128,G__3129,seq3127__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.removeFromEntity = (function czlab$elmo$afx$ecs$removeFromEntity(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3150 = arguments.length;
var i__4500__auto___3151 = (0);
while(true){
if((i__4500__auto___3151 < len__4499__auto___3150)){
args__4502__auto__.push((arguments[i__4500__auto___3151]));

var G__3152 = (i__4500__auto___3151 + (1));
i__4500__auto___3151 = G__3152;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.removeFromEntity.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.removeFromEntity.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,entity,componentIds){
cljs.core.swap_BANG_.call(null,ecs,(function (p__3137){
var map__3138 = p__3137;
var map__3138__$1 = ((((!((map__3138 == null)))?(((((map__3138.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__3138.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__3138):map__3138);
var root = map__3138__$1;
var data = cljs.core.get.call(null,map__3138__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var dt = (function (){var dtree = data;
var G__3143 = componentIds;
var vec__3144 = G__3143;
var seq__3145 = cljs.core.seq.call(null,vec__3144);
var first__3146 = cljs.core.first.call(null,seq__3145);
var seq__3145__$1 = cljs.core.next.call(null,seq__3145);
var cid = first__3146;
var xs = seq__3145__$1;
var dtree__$1 = dtree;
var G__3143__$1 = G__3143;
while(true){
var dtree__$2 = dtree__$1;
var vec__3147 = G__3143__$1;
var seq__3148 = cljs.core.seq.call(null,vec__3147);
var first__3149 = cljs.core.first.call(null,seq__3148);
var seq__3148__$1 = cljs.core.next.call(null,seq__3148);
var cid__$1 = first__3149;
var xs__$1 = seq__3148__$1;
if(!(!((cid__$1 == null)))){
return dtree__$2;
} else {
var co = cljs.core.get.call(null,dtree__$2,cid__$1);
var G__3153 = (function (){var temp__5459__auto__ = ((!((co == null)))?cljs.core.get.call(null,co,entity):null);
if((temp__5459__auto__ == null)){
return dtree__$2;
} else {
var v = temp__5459__auto__;
czlab.elmo.afx.ecs.retUsed.call(null,v);

return cljs.core.update_in.call(null,dtree__$2,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cid__$1], null),((function (dtree__$1,G__3143__$1,v,temp__5459__auto__,co,dtree__$2,vec__3147,seq__3148,first__3149,seq__3148__$1,cid__$1,xs__$1,dtree,G__3143,vec__3144,seq__3145,first__3146,seq__3145__$1,cid,xs,map__3138,map__3138__$1,root,data){
return (function (p1__3133_SHARP_){
return cljs.core.dissoc.call(null,p1__3133_SHARP_,entity);
});})(dtree__$1,G__3143__$1,v,temp__5459__auto__,co,dtree__$2,vec__3147,seq__3148,first__3149,seq__3148__$1,cid__$1,xs__$1,dtree,G__3143,vec__3144,seq__3145,first__3146,seq__3145__$1,cid,xs,map__3138,map__3138__$1,root,data))
);
}
})();
var G__3154 = xs__$1;
dtree__$1 = G__3153;
G__3143__$1 = G__3154;
continue;
}
break;
}
})();
if((dt === data)){
return root;
} else {
return cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"data","data",-232669377),dt);
}
}));

return ecs;
});

czlab.elmo.afx.ecs.removeFromEntity.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.removeFromEntity.cljs$lang$applyTo = (function (seq3134){
var G__3135 = cljs.core.first.call(null,seq3134);
var seq3134__$1 = cljs.core.next.call(null,seq3134);
var G__3136 = cljs.core.first.call(null,seq3134__$1);
var seq3134__$2 = cljs.core.next.call(null,seq3134__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3135,G__3136,seq3134__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.getEntityData = (function czlab$elmo$afx$ecs$getEntityData(ecs,entity,componentId){
var temp__5459__auto__ = cljs.core.get.call(null,new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)),componentId);
if((temp__5459__auto__ == null)){
return null;
} else {
var ct = temp__5459__auto__;
return cljs.core.get.call(null,ct,entity);
}
});
/**
 * 
 */
czlab.elmo.afx.ecs.updateEntity = (function czlab$elmo$afx$ecs$updateEntity(ecs,entity,componentId,func){
var temp__5461__auto__ = czlab.elmo.afx.ecs.getEntityData.call(null,ecs,entity,componentId);
if((temp__5461__auto__ == null)){
return null;
} else {
var c = temp__5461__auto__;
func.call(null,c);

return c;
}
});
/**
 * 
 */
czlab.elmo.afx.ecs.getComponentsData = (function czlab$elmo$afx$ecs$getComponentsData(ecs,componentId){
var temp__5459__auto__ = cljs.core.get.call(null,new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)),componentId);
if((temp__5459__auto__ == null)){
return cljs.core.PersistentVector.EMPTY;
} else {
var c = temp__5459__auto__;
return cljs.core.vals.call(null,c);
}
});
/**
 * 
 */
czlab.elmo.afx.ecs.getComponentKeys = (function czlab$elmo$afx$ecs$getComponentKeys(ecs){
return cljs.core.keys.call(null,new cljs.core.Keyword(null,"registry","registry",1021159018).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)));
});
/**
 * 
 */
czlab.elmo.afx.ecs.findComponent = (function czlab$elmo$afx$ecs$findComponent(ecs,componentId){
return cljs.core.get.call(null,new cljs.core.Keyword(null,"registry","registry",1021159018).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)),componentId);
});
/**
 * 
 */
czlab.elmo.afx.ecs.componentInEntity_QMARK_ = (function czlab$elmo$afx$ecs$componentInEntity_QMARK_(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3160 = arguments.length;
var i__4500__auto___3161 = (0);
while(true){
if((i__4500__auto___3161 < len__4499__auto___3160)){
args__4502__auto__.push((arguments[i__4500__auto___3161]));

var G__3162 = (i__4500__auto___3161 + (1));
i__4500__auto___3161 = G__3162;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((3) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((3)),(0),null)):null);
return czlab.elmo.afx.ecs.componentInEntity_QMARK_.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.componentInEntity_QMARK_.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,entity,componentId,moreIds){
var d = new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs));
return cljs.core.not.call(null,cljs.core.some.call(null,((function (d){
return (function (p1__3155_SHARP_){
var temp__5459__auto__ = cljs.core.get.call(null,d,p1__3155_SHARP_);
if((temp__5459__auto__ == null)){
return true;
} else {
var co = temp__5459__auto__;
return !(cljs.core.contains_QMARK_.call(null,co,entity));
}
});})(d))
,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [componentId], null),moreIds)));
});

czlab.elmo.afx.ecs.componentInEntity_QMARK_.cljs$lang$maxFixedArity = (3);

/** @this {Function} */
czlab.elmo.afx.ecs.componentInEntity_QMARK_.cljs$lang$applyTo = (function (seq3156){
var G__3157 = cljs.core.first.call(null,seq3156);
var seq3156__$1 = cljs.core.next.call(null,seq3156);
var G__3158 = cljs.core.first.call(null,seq3156__$1);
var seq3156__$2 = cljs.core.next.call(null,seq3156__$1);
var G__3159 = cljs.core.first.call(null,seq3156__$2);
var seq3156__$3 = cljs.core.next.call(null,seq3156__$2);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3157,G__3158,G__3159,seq3156__$3);
});

/**
 * 
 */
czlab.elmo.afx.ecs.findEntities = (function czlab$elmo$afx$ecs$findEntities(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3187 = arguments.length;
var i__4500__auto___3188 = (0);
while(true){
if((i__4500__auto___3188 < len__4499__auto___3187)){
args__4502__auto__.push((arguments[i__4500__auto___3188]));

var G__3189 = (i__4500__auto___3188 + (1));
i__4500__auto___3188 = G__3189;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.findEntities.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.findEntities.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,componentId,moreIds){
var data = new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs));
var ret = cljs.core.atom.call(null,cljs.core.PersistentVector.EMPTY);
var cids = cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [componentId], null),moreIds);
if(cljs.core.every_QMARK_.call(null,((function (data,ret,cids){
return (function (p1__3163_SHARP_){
return cljs.core.contains_QMARK_.call(null,data,p1__3163_SHARP_);
});})(data,ret,cids))
,cids)){
var ccs_3190 = cljs.core.sort.call(null,((function (data,ret,cids){
return (function (p1__3164_SHARP_,p2__3165_SHARP_){
if((cljs.core.first.call(null,p1__3164_SHARP_) < cljs.core.first.call(null,p2__3165_SHARP_))){
return (-1);
} else {
if((cljs.core.first.call(null,p1__3164_SHARP_) > cljs.core.first.call(null,p2__3165_SHARP_))){
return (1);
} else {
return (0);

}
}
});})(data,ret,cids))
,cljs.core.map.call(null,((function (data,ret,cids){
return (function (p1__3166_SHARP_){
var v = cljs.core.get.call(null,data,p1__3166_SHARP_);
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.count.call(null,v),v], null);
});})(data,ret,cids))
,cids));
var ccsz_3191 = cljs.core.count.call(null,ccs_3190);
var vec__3170_3192 = (((ccsz_3191 > (0)))?cljs.core.first.call(null,ccs_3190):null);
var __3193 = cljs.core.nth.call(null,vec__3170_3192,(0),null);
var c0_3194 = cljs.core.nth.call(null,vec__3170_3192,(1),null);
if(!((c0_3194 == null))){
var seq__3173_3195 = cljs.core.seq.call(null,cljs.core.keys.call(null,c0_3194));
var chunk__3175_3196 = null;
var count__3176_3197 = (0);
var i__3177_3198 = (0);
while(true){
if((i__3177_3198 < count__3176_3197)){
var eid_3199 = cljs.core._nth.call(null,chunk__3175_3196,i__3177_3198);
var sum_3200 = cljs.core.atom.call(null,(0));
var seq__3179_3201 = cljs.core.seq.call(null,ccs_3190);
var chunk__3180_3202 = null;
var count__3181_3203 = (0);
var i__3182_3204 = (0);
while(true){
if((i__3182_3204 < count__3181_3203)){
var c_3205 = cljs.core._nth.call(null,chunk__3180_3202,i__3182_3204);
if((((c_3205 === c0_3194)) || (cljs.core.contains_QMARK_.call(null,c_3205,eid_3199)))){
cljs.core.swap_BANG_.call(null,sum_3200,cljs.core.inc);
} else {
}


var G__3206 = seq__3179_3201;
var G__3207 = chunk__3180_3202;
var G__3208 = count__3181_3203;
var G__3209 = (i__3182_3204 + (1));
seq__3179_3201 = G__3206;
chunk__3180_3202 = G__3207;
count__3181_3203 = G__3208;
i__3182_3204 = G__3209;
continue;
} else {
var temp__5457__auto___3210 = cljs.core.seq.call(null,seq__3179_3201);
if(temp__5457__auto___3210){
var seq__3179_3211__$1 = temp__5457__auto___3210;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__3179_3211__$1)){
var c__4319__auto___3212 = cljs.core.chunk_first.call(null,seq__3179_3211__$1);
var G__3213 = cljs.core.chunk_rest.call(null,seq__3179_3211__$1);
var G__3214 = c__4319__auto___3212;
var G__3215 = cljs.core.count.call(null,c__4319__auto___3212);
var G__3216 = (0);
seq__3179_3201 = G__3213;
chunk__3180_3202 = G__3214;
count__3181_3203 = G__3215;
i__3182_3204 = G__3216;
continue;
} else {
var c_3217 = cljs.core.first.call(null,seq__3179_3211__$1);
if((((c_3217 === c0_3194)) || (cljs.core.contains_QMARK_.call(null,c_3217,eid_3199)))){
cljs.core.swap_BANG_.call(null,sum_3200,cljs.core.inc);
} else {
}


var G__3218 = cljs.core.next.call(null,seq__3179_3211__$1);
var G__3219 = null;
var G__3220 = (0);
var G__3221 = (0);
seq__3179_3201 = G__3218;
chunk__3180_3202 = G__3219;
count__3181_3203 = G__3220;
i__3182_3204 = G__3221;
continue;
}
} else {
}
}
break;
}

if(cljs.core._EQ_.call(null,cljs.core.deref.call(null,sum_3200),ccsz_3191)){
cljs.core.swap_BANG_.call(null,ret,cljs.core.conj,eid_3199);
} else {
}


var G__3222 = seq__3173_3195;
var G__3223 = chunk__3175_3196;
var G__3224 = count__3176_3197;
var G__3225 = (i__3177_3198 + (1));
seq__3173_3195 = G__3222;
chunk__3175_3196 = G__3223;
count__3176_3197 = G__3224;
i__3177_3198 = G__3225;
continue;
} else {
var temp__5457__auto___3226 = cljs.core.seq.call(null,seq__3173_3195);
if(temp__5457__auto___3226){
var seq__3173_3227__$1 = temp__5457__auto___3226;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__3173_3227__$1)){
var c__4319__auto___3228 = cljs.core.chunk_first.call(null,seq__3173_3227__$1);
var G__3229 = cljs.core.chunk_rest.call(null,seq__3173_3227__$1);
var G__3230 = c__4319__auto___3228;
var G__3231 = cljs.core.count.call(null,c__4319__auto___3228);
var G__3232 = (0);
seq__3173_3195 = G__3229;
chunk__3175_3196 = G__3230;
count__3176_3197 = G__3231;
i__3177_3198 = G__3232;
continue;
} else {
var eid_3233 = cljs.core.first.call(null,seq__3173_3227__$1);
var sum_3234 = cljs.core.atom.call(null,(0));
var seq__3183_3235 = cljs.core.seq.call(null,ccs_3190);
var chunk__3184_3236 = null;
var count__3185_3237 = (0);
var i__3186_3238 = (0);
while(true){
if((i__3186_3238 < count__3185_3237)){
var c_3239 = cljs.core._nth.call(null,chunk__3184_3236,i__3186_3238);
if((((c_3239 === c0_3194)) || (cljs.core.contains_QMARK_.call(null,c_3239,eid_3233)))){
cljs.core.swap_BANG_.call(null,sum_3234,cljs.core.inc);
} else {
}


var G__3240 = seq__3183_3235;
var G__3241 = chunk__3184_3236;
var G__3242 = count__3185_3237;
var G__3243 = (i__3186_3238 + (1));
seq__3183_3235 = G__3240;
chunk__3184_3236 = G__3241;
count__3185_3237 = G__3242;
i__3186_3238 = G__3243;
continue;
} else {
var temp__5457__auto___3244__$1 = cljs.core.seq.call(null,seq__3183_3235);
if(temp__5457__auto___3244__$1){
var seq__3183_3245__$1 = temp__5457__auto___3244__$1;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__3183_3245__$1)){
var c__4319__auto___3246 = cljs.core.chunk_first.call(null,seq__3183_3245__$1);
var G__3247 = cljs.core.chunk_rest.call(null,seq__3183_3245__$1);
var G__3248 = c__4319__auto___3246;
var G__3249 = cljs.core.count.call(null,c__4319__auto___3246);
var G__3250 = (0);
seq__3183_3235 = G__3247;
chunk__3184_3236 = G__3248;
count__3185_3237 = G__3249;
i__3186_3238 = G__3250;
continue;
} else {
var c_3251 = cljs.core.first.call(null,seq__3183_3245__$1);
if((((c_3251 === c0_3194)) || (cljs.core.contains_QMARK_.call(null,c_3251,eid_3233)))){
cljs.core.swap_BANG_.call(null,sum_3234,cljs.core.inc);
} else {
}


var G__3252 = cljs.core.next.call(null,seq__3183_3245__$1);
var G__3253 = null;
var G__3254 = (0);
var G__3255 = (0);
seq__3183_3235 = G__3252;
chunk__3184_3236 = G__3253;
count__3185_3237 = G__3254;
i__3186_3238 = G__3255;
continue;
}
} else {
}
}
break;
}

if(cljs.core._EQ_.call(null,cljs.core.deref.call(null,sum_3234),ccsz_3191)){
cljs.core.swap_BANG_.call(null,ret,cljs.core.conj,eid_3233);
} else {
}


var G__3256 = cljs.core.next.call(null,seq__3173_3227__$1);
var G__3257 = null;
var G__3258 = (0);
var G__3259 = (0);
seq__3173_3195 = G__3256;
chunk__3175_3196 = G__3257;
count__3176_3197 = G__3258;
i__3177_3198 = G__3259;
continue;
}
} else {
}
}
break;
}
} else {
}
} else {
}

return cljs.core.deref.call(null,ret);
});

czlab.elmo.afx.ecs.findEntities.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.findEntities.cljs$lang$applyTo = (function (seq3167){
var G__3168 = cljs.core.first.call(null,seq3167);
var seq3167__$1 = cljs.core.next.call(null,seq3167);
var G__3169 = cljs.core.first.call(null,seq3167__$1);
var seq3167__$2 = cljs.core.next.call(null,seq3167__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3168,G__3169,seq3167__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.addTemplate = (function czlab$elmo$afx$ecs$addTemplate(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3265 = arguments.length;
var i__4500__auto___3266 = (0);
while(true){
if((i__4500__auto___3266 < len__4499__auto___3265)){
args__4502__auto__.push((arguments[i__4500__auto___3266]));

var G__3267 = (i__4500__auto___3266 + (1));
i__4500__auto___3266 = G__3267;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((3) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((3)),(0),null)):null);
return czlab.elmo.afx.ecs.addTemplate.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.addTemplate.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,id,template,more){
cljs.core.swap_BANG_.call(null,ecs,(function (p1__3260_SHARP_){
return cljs.core.assoc.call(null,p1__3260_SHARP_,new cljs.core.Keyword(null,"templates","templates",-1237401733),cljs.core.merge.call(null,new cljs.core.Keyword(null,"templates","templates",-1237401733).cljs$core$IFn$_invoke$arity$1(p1__3260_SHARP_),cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,cljs.core.partition.call(null,(2),cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [id,template], null),more)))));
}));

return ecs;
});

czlab.elmo.afx.ecs.addTemplate.cljs$lang$maxFixedArity = (3);

/** @this {Function} */
czlab.elmo.afx.ecs.addTemplate.cljs$lang$applyTo = (function (seq3261){
var G__3262 = cljs.core.first.call(null,seq3261);
var seq3261__$1 = cljs.core.next.call(null,seq3261);
var G__3263 = cljs.core.first.call(null,seq3261__$1);
var seq3261__$2 = cljs.core.next.call(null,seq3261__$1);
var G__3264 = cljs.core.first.call(null,seq3261__$2);
var seq3261__$3 = cljs.core.next.call(null,seq3261__$2);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3262,G__3263,G__3264,seq3261__$3);
});

/**
 * 
 */
czlab.elmo.afx.ecs.getTemplateKeys = (function czlab$elmo$afx$ecs$getTemplateKeys(ecs){
return cljs.core.keys.call(null,new cljs.core.Keyword(null,"templates","templates",-1237401733).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)));
});
/**
 * 
 */
czlab.elmo.afx.ecs.findTemplate = (function czlab$elmo$afx$ecs$findTemplate(ecs,templateId){
return cljs.core.get.call(null,new cljs.core.Keyword(null,"templates","templates",-1237401733).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)),templateId);
});
/**
 * 
 */
czlab.elmo.afx.ecs.removeTemplate = (function czlab$elmo$afx$ecs$removeTemplate(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3272 = arguments.length;
var i__4500__auto___3273 = (0);
while(true){
if((i__4500__auto___3273 < len__4499__auto___3272)){
args__4502__auto__.push((arguments[i__4500__auto___3273]));

var G__3274 = (i__4500__auto___3273 + (1));
i__4500__auto___3273 = G__3274;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.removeTemplate.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.removeTemplate.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,id,moreIds){
cljs.core.swap_BANG_.call(null,ecs,(function (p1__3268_SHARP_){
return cljs.core.assoc.call(null,p1__3268_SHARP_,new cljs.core.Keyword(null,"templates","templates",-1237401733),cljs.core.apply.call(null,cljs.core.dissoc,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"templates","templates",-1237401733).cljs$core$IFn$_invoke$arity$1(p1__3268_SHARP_),id], null),moreIds)));
}));

return ecs;
});

czlab.elmo.afx.ecs.removeTemplate.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.removeTemplate.cljs$lang$applyTo = (function (seq3269){
var G__3270 = cljs.core.first.call(null,seq3269);
var seq3269__$1 = cljs.core.next.call(null,seq3269);
var G__3271 = cljs.core.first.call(null,seq3269__$1);
var seq3269__$2 = cljs.core.next.call(null,seq3269__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3270,G__3271,seq3269__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.createTemplateEntity = (function czlab$elmo$afx$ecs$createTemplateEntity(ecs,id){
var map__3275 = cljs.core.get.call(null,new cljs.core.Keyword(null,"templates","templates",-1237401733).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)),id);
var map__3275__$1 = ((((!((map__3275 == null)))?(((((map__3275.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__3275.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__3275):map__3275);
var t = map__3275__$1;
var components = cljs.core.get.call(null,map__3275__$1,new cljs.core.Keyword(null,"components","components",-1073188942));
var initor = cljs.core.get.call(null,map__3275__$1,new cljs.core.Keyword(null,"initor","initor",-1477668576));
var e = ((!((t == null)))?cljs.core.apply.call(null,czlab.elmo.afx.ecs.createEntity,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [ecs], null),components)):null);
if(((!((e == null))) && (cljs.core.fn_QMARK_.call(null,initor)))){
initor.call(null,ecs,e);
} else {
}

return e;
});
/**
 * 
 */
czlab.elmo.afx.ecs.addSystem = (function czlab$elmo$afx$ecs$addSystem(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3281 = arguments.length;
var i__4500__auto___3282 = (0);
while(true){
if((i__4500__auto___3282 < len__4499__auto___3281)){
args__4502__auto__.push((arguments[i__4500__auto___3282]));

var G__3283 = (i__4500__auto___3282 + (1));
i__4500__auto___3282 = G__3283;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.addSystem.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.addSystem.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,system,more){
cljs.core.swap_BANG_.call(null,ecs,(function (p1__3277_SHARP_){
return cljs.core.assoc.call(null,p1__3277_SHARP_,new cljs.core.Keyword(null,"systems","systems",-1015374944),cljs.core.concat.call(null,new cljs.core.Keyword(null,"systems","systems",-1015374944).cljs$core$IFn$_invoke$arity$1(p1__3277_SHARP_),cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [system], null),more)));
}));

return ecs;
});

czlab.elmo.afx.ecs.addSystem.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.addSystem.cljs$lang$applyTo = (function (seq3278){
var G__3279 = cljs.core.first.call(null,seq3278);
var seq3278__$1 = cljs.core.next.call(null,seq3278);
var G__3280 = cljs.core.first.call(null,seq3278__$1);
var seq3278__$2 = cljs.core.next.call(null,seq3278__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3279,G__3280,seq3278__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.removeSystem = (function czlab$elmo$afx$ecs$removeSystem(var_args){
var args__4502__auto__ = [];
var len__4499__auto___3288 = arguments.length;
var i__4500__auto___3289 = (0);
while(true){
if((i__4500__auto___3289 < len__4499__auto___3288)){
args__4502__auto__.push((arguments[i__4500__auto___3289]));

var G__3290 = (i__4500__auto___3289 + (1));
i__4500__auto___3289 = G__3290;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((2) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((2)),(0),null)):null);
return czlab.elmo.afx.ecs.removeSystem.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__4503__auto__);
});

czlab.elmo.afx.ecs.removeSystem.cljs$core$IFn$_invoke$arity$variadic = (function (ecs,system,more){
var yy = cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [system], null),more);
cljs.core.swap_BANG_.call(null,ecs,((function (yy){
return (function (root){
return cljs.core.assoc.call(null,root,new cljs.core.Keyword(null,"systems","systems",-1015374944),cljs.core.filter.call(null,((function (yy){
return (function (s){
return cljs.core.not_any_QMARK_.call(null,((function (yy){
return (function (p1__3284_SHARP_){
return (p1__3284_SHARP_ === s);
});})(yy))
,yy);
});})(yy))
,new cljs.core.Keyword(null,"systems","systems",-1015374944).cljs$core$IFn$_invoke$arity$1(root)));
});})(yy))
);

return ecs;
});

czlab.elmo.afx.ecs.removeSystem.cljs$lang$maxFixedArity = (2);

/** @this {Function} */
czlab.elmo.afx.ecs.removeSystem.cljs$lang$applyTo = (function (seq3285){
var G__3286 = cljs.core.first.call(null,seq3285);
var seq3285__$1 = cljs.core.next.call(null,seq3285);
var G__3287 = cljs.core.first.call(null,seq3285__$1);
var seq3285__$2 = cljs.core.next.call(null,seq3285__$1);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__3286,G__3287,seq3285__$2);
});

/**
 * 
 */
czlab.elmo.afx.ecs.updateECS = (function czlab$elmo$afx$ecs$updateECS(ecs,dt){
var seq__3291_3295 = cljs.core.seq.call(null,new cljs.core.Keyword(null,"systems","systems",-1015374944).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,ecs)));
var chunk__3292_3296 = null;
var count__3293_3297 = (0);
var i__3294_3298 = (0);
while(true){
if((i__3294_3298 < count__3293_3297)){
var s_3299 = cljs.core._nth.call(null,chunk__3292_3296,i__3294_3298);
s_3299.call(null,ecs,dt);


var G__3300 = seq__3291_3295;
var G__3301 = chunk__3292_3296;
var G__3302 = count__3293_3297;
var G__3303 = (i__3294_3298 + (1));
seq__3291_3295 = G__3300;
chunk__3292_3296 = G__3301;
count__3293_3297 = G__3302;
i__3294_3298 = G__3303;
continue;
} else {
var temp__5457__auto___3304 = cljs.core.seq.call(null,seq__3291_3295);
if(temp__5457__auto___3304){
var seq__3291_3305__$1 = temp__5457__auto___3304;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__3291_3305__$1)){
var c__4319__auto___3306 = cljs.core.chunk_first.call(null,seq__3291_3305__$1);
var G__3307 = cljs.core.chunk_rest.call(null,seq__3291_3305__$1);
var G__3308 = c__4319__auto___3306;
var G__3309 = cljs.core.count.call(null,c__4319__auto___3306);
var G__3310 = (0);
seq__3291_3295 = G__3307;
chunk__3292_3296 = G__3308;
count__3293_3297 = G__3309;
i__3294_3298 = G__3310;
continue;
} else {
var s_3311 = cljs.core.first.call(null,seq__3291_3305__$1);
s_3311.call(null,ecs,dt);


var G__3312 = cljs.core.next.call(null,seq__3291_3305__$1);
var G__3313 = null;
var G__3314 = (0);
var G__3315 = (0);
seq__3291_3295 = G__3312;
chunk__3292_3296 = G__3313;
count__3293_3297 = G__3314;
i__3294_3298 = G__3315;
continue;
}
} else {
}
}
break;
}

return ecs;
});

//# sourceMappingURL=ecs.js.map
