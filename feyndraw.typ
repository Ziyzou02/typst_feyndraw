#import "@preview/cetz:0.2.2": canvas, draw,

#import draw: circle, arc, bezier,line,mark,content, fill,

#import draw:rotate,group
// some vector calculation

#let add(a, b) = {
  if a.len() != b.len() {
    return [Error length]
  } else{
    a.zip(b).map(
      ((a,b))=> (a+b)
    )
    }
  }

#let minu(a,b) = add(a,b.map(t=>-t))

#let scal(v, b) = v.map(t=> t*b) 

#let norm(a) = {
  let s = 0
  for x in a{s = s + x*x}
  return calc.sqrt(s)
}

#let vec_sum(a) = {
  let s = 0
  for k in a {s=s+k}
  return s
}

#let inner_product(a,b) = vec_sum(a.zip(b).map(((a,b))=> a*b))

#let sgn(a) = if a!=0 {a/calc.abs(a)}else{ 0}

#let unit(a) = scal(a,1/norm(a))

#let rot_vec_2dim(ang, a) = {
  (calc.cos(ang)*a.at(0)- 
   calc.sin(ang)*a.at(1),
   calc.sin(ang)*a.at(0) +   
   calc.cos(ang)*a.at(1))
}

#let arc_angel2crd(position, ang, radius,)={
  ( position.at(0) + radius * calc.cos(ang),position.at(0) + radius * calc.sin(ang)
  )
}

#let crd2angel(position, crd )={
  let dx = crd.at(0) - position.at(0)
  let dy = crd.at(1) - position.at(1)
  let k = 0
  if dx == 0 {
    k = sgn(dy)*calc.inf
  } else {
    k = dy/dx
  }
  let a = calc.atan(k)
  if dx < 0{
    a = a + 180deg
  }else{
    if dy < 0{
      a = a + 360deg
    }
  }
  if a < 0deg {
    return a + 360deg
  }else{return a}
}
// crd2angle 给出坐标crd相对于position的角度，值域为[0,360)deg

#let ppa_circle_crd(a,b,ang,re:1)={
// ppa : means "point-point-angle" use these 3 variables to determine an arc.
// crd: means coordinate. 
// a, b: two point crd
// ang: a,b两点张成的圆心角
//  re: 两点一角有两种可能的取向，从短边来看，1为逆时针,-1为顺时针。
// return: radius and crd of circle(crd) center. (radius, (xc, yc) )
let Q = re*calc.abs((1 + calc.cos(ang))/calc.sin(ang))
  let r = 1/2 *calc.sqrt(calc.pow(norm(minu(a,b)),2)*(1+calc.pow(Q,2)))
  let xc = a.at(0) - 1/2 * inner_product(minu(a,b), (1,-Q))
  let yc = a.at(1) - 1/2 *inner_product(minu(a,b), (Q,1)) 
  return (r,(xc, yc))
}

#let arc_spilt(a, b, angle:calc.inf,re:1,node:8)={
  // a, b, angle,re: same as ppa_circle_crd()
  // 将弧线分为若干个节点，并给出这些节点的坐标。
  let s = ()
  if angle == calc.inf{
    let c = unit(minu(b, a))
    let l = norm(minu(b, a))/node
    let lvec = scal(c, l)
    let start_p = a
    s = s + (start_p,)
    for i in range(node){
    start_p = add(start_p, lvec)
    s = s + (start_p,)
    }
    return s
  }else{
  let rcc = ppa_circle_crd(a, b, angle, re:re)
  let r= rcc.at(0)
  let cc = rcc.at(1)
  let a_angel = crd2angel(cc, a)       
  let b_angel = crd2angel(cc, b)
  if b_angel < a_angel {b_angel = b_angel+360deg}
  let start_angel = a_angel
  let end_angel = b_angel
  angle = end_angel - start_angel
  
  if node < 2 {Error("node must >= 2")}
  let d_angel = angle/(node - 1)
  for i in range(0,node){
    let ang = start_angel + i * d_angel
    let unit_vec = (calc.cos(ang),calc.sin(ang))
    s = s + (add(cc, scal(unit_vec, r)),)
  }
  return s
}}

#let polar_crd(r, angle) = {
  (r*calc.cos(angle),r*calc.sin(angle))
}

#let para_grad(t, func, e:0.000000002)={
  // func(t) = (x(t), y(t))
  // return the grad of point f(t)
  if e * t < 0 * t {
    e = -e
  }
  let dp = minu(func(t + e*t) , func(t - e*t))
  return crd2angel((0,0),dp)
}

#let tangent(..args)={
  return polar_crd(1,para_grad(..args))
}

#let tangent_show(..args, r:0.01,storke:none, symbol:"stealth", mark_pos:"end",line_show:true)={
  let tgt = tangent(..args)
  let start = args.pos().at(1)(args.pos().at(0))
  let end = add(start, scal(tgt,r))
  if line_show != false{
  line(start, end, storke:storke)}
  if mark_pos == "end"{
  mark(start, end,symbol:symbol,fill:black)}
  else if mark_pos == "mid"{
    mark(
      start, 
      add(start, scal(tgt,.7*r)),
      symbol:symbol, fill:black
    )
  }else if mark_pos == "start"{
    mark(
      start, 
      add(start, scal(tgt,.1*r)),
      symbol:symbol, fill:black
    )
  }
  
}


// shape 
shape
#let dot(x,y,r:.1, fill:black) = {
  circle((x,y),radius: r,fill: fill)
  }
#let Dot(position, r:.05,fill:black)=circle(position,radius: r,fill: fill)

#let waveline_group(start:(), end:(), node: 10)={
  let q = 1
  let re = 1
    let long=scal(minu(end,start),1/node)
    let Tran= rot_vec_2dim(-90deg, long)
  while q <= node {
    let a = add(start , long.map(t=>t*(q - 1)))
    let b = add(a , long)
    Tran = Tran.map(t=>-1*t)
    q = q + 1
    re = re * (-1)
    (
      bezier(a, b, add(scal(add(a,b),1/2), Tran.map(t=>t*1.4))),
    )
  }
} 

#let twopoint_wave(a, b, orien:1, amp:1.2)={
  let long=scal(minu(b,a),1)
  let tran = add(long,scal(rot_vec_2dim(orien*90, long),amp))
  bezier(a, b,add(a, tran))
}

#let waveline(starts, ends, node:10)={
  for i in waveline_group(start:starts,end:ends,node:node) {i}
}


#let ppa_arc(a, b, ang, re:1, center_show: true)={
  // 
  if ang == calc.inf{
    line(a, b)
  }else{
  let r_crd = ppa_circle_crd(a,b,ang)
  let r = r_crd.at(0)
  let cc = r_crd.at(1)

  let a_ang = crd2angel(cc,a)
  let b_ang = crd2angel(cc,b)
  if b_ang < a_ang{
    b_ang = b_ang + 360deg
  }

  // let start_p(i) = (a, b).at(i)
  // let start_ang = calc.min(a_ang, b_ang)
  // let stop_ang = calc.max(a_ang,b_ang)
  // let i = 0
  // if start_ang == b_ang {
  //    i = 1
  // }
  arc(a, start: a_ang, stop: b_ang ,radius:r)
  if center_show{
  circle(cc,radius:.05,fill:gray)
  }
}}

#let  no_match_cir(a, b,rate:2)={
  let long = minu(a,b)
  let c = add(a,scal(long,1/rate))
  ppa_arc(a, c,180deg,center_show: false)

  ppa_arc(c, b, 180deg,center_show: false)
}

#let elem_gluon(a, b,re:1)={
  let d = minu(b, a)
  let t = rot_vec_2dim(90deg, d)
  let r1 = 0.7 // r1 > 0.5
  let r2 = r1 - 0.5
  let R1 = norm(d) * r1
  let R2 = norm(d) * r2
  let v1 = add(a,scal(add(d,t),r1))
  
  let v2 = add(a,add(scal(d, 1 - r1),scal(t,r1)))
  ppa_arc(a, v1, 90deg,center_show: false)
  ppa_arc(v1, v2, 180deg, center_show: false)
  ppa_arc(v2, b, 90deg, center_show: false)
  // arc(a, start:-90deg * re, stop: 0deg, radius: R1)
  // arc(add(a,(R1,re*R1)), start:0deg, stop:180deg*re, radius:R2)
  // arc(add(a,(R1 - 2*R2, re*R1)),start:-180deg * re, stop:-90deg * re, radius:R1)
}

#let gluon(a, b, angle: calc.inf,re:1, node:12,vertex_show:false)={
    let d = minu(b, a)
    let long = unit(d)
    let start_point = a
    let end_point = b
    line(a, start_point)
    line(b, end_point)
    let point_group = arc_spilt(start_point, end_point, angle: angle, re:re, node:node)
    for i in range(point_group.len()-1){
      elem_gluon(point_group.at(i), point_group.at(i+1))
    }
    if vertex_show{
    Dot(a,r:.02)
    Dot(b,r:.02)
    }
}


#let ph(a,b,angle:calc.inf, orien: 1,node:12, vertex_show:false,m:"")={
  if angle == 0deg{
    angle = 1deg
  }
  
  if calc.abs(b.at(0) - a.at(0)) <= 0.1{
    orien = -sgn(a.at(1)-b.at(1))
  }
  if orien == 1{
    let t = a 
    a = b
    b = t}
  let q = arc_spilt(a,b,angle: angle,node:node)
  let f = 1
  for i in range(q.len()-1){
twopoint_wave(q.at(i),q.at(i+1),orien: f)
  f = f * -1
  }
  if vertex_show{
    Dot(a)
    Dot(b)
  }
  if m != "" {
    let tran = rot_vec_2dim(-90deg,scal(minu(b,a),0.2))
    let m_position = add(tran,q.at(calc.floor(node/2)))
content(m_position)[#emph(m)]}
}

#let fermion(a, b, ang:calc.inf, re:1,m:"",m_show:true, mark_show: true)={
  // the curved fermion_line havn't be done,you can use ppa_arc() and mark() to replace
  line(a, b)
  let long = scal(minu(b,a),5/8)
  let tran =  scal(rot_vec_2dim(-90,long), re/4)
  if mark_show{
  mark(a, add(a,long),symbol:">",fill:black)}
  if m != "" and m_show{
    content(add(add(a,long),tran))[#emph(m)]
  }
}

#let dashed_line(a, b, node:6, d:0.08, stroke:none)={
  let c = minu(b, a)
  let l = (1 - (node - 1)*d)/node
  let lvec = scal(c, l)
  let dvec = scal(c, d)
  let start_p = a
  let end_p = add(a, lvec)
  for i in range(node){
    line(start_p, end_p,stroke:stroke)
    start_p = add(end_p, dvec)
    end_p = add(start_p, lvec)
  }
}

#let scalar(a, b, angle:calc.inf,re:1, form: 1, node:6, vertex_show:false, m: "")={
  if form == 1{
    ppa_arc(a, b, angle,re:re, center_show: false)
    }else{
  let point_group = arc_spilt(a, b, angle: angle, node:2*node, re: re)
  for i in range(point_group.len()-1, step: 2){
    line(point_group.at(i),
        point_group.at(i+1))
    }
    }
  if vertex_show{
    Dot(a)
    Dot(b)
  }
  if m != ""{
  let long = scal(minu(b,a),5/8)
  let tran =  scal(rot_vec_2dim(-90,long), re/2)
    content(add(add(a,long),tran))[#emph(m)]
  }
  }


#let oval(c, long:1, width:1, fill:white)={
  scale(y:long, x:width)
  circle(c,fill:fill)
  scale(y:1/long, x:1/width)
}

#let axis(o, xlim:(-4,4),ylim:(-4,4),ticks:false,xticks:false, yticks:false)={
  Dot(o)
  line((xlim.at(0),0),
  (xlim.at(1),0))
  line((0,ylim.at(0)),
  (0,ylim.at(1)))
  mark(o, (xlim.at(1),0),symbol:">",fill:black)
  mark(o, (0,ylim.at(1)),symbol:">",fill:black)
  if ticks{
    xticks=true
    yticks=true
  }
  if xticks{
    for i in range(calc.floor(xlim.at(0))+1,calc.floor(xlim.at(1))){
      dot(i,0,r:0.05)
    }
  }
  if yticks{
    for i in range(calc.floor(ylim.at(0))+1,calc.floor(ylim.at(1))){
      dot(0,i,r:0.05)
    }
  }
}

// some method for complex diagrams

#let fermion_loop(a, b,vertex_show:false)={
  ppa_arc(a, b, 180deg, center_show:false)
  ppa_arc(b, a, 180deg, center_show:false)
  let long = scal(minu(b, a),0.5)
  let tran = rot_vec_2dim( 90deg,long)
  let v1 = add(a, tran)
  let v2 = add(v1,scal(long,1.2))
  mark(v1, v2,symbol:">",fill:black)
  v1 = minu(b, tran)
  v2 = minu(v1, scal(long, 1.2))
mark(v1, v2,symbol:">",fill:black)
  if vertex_show{
  Dot(a)
  Dot(b)}

}

// Custom Elements
Custom Elements
#let ox(position, r,angle:45deg)={
  circle(position, radius:r)
  let s1 = add(position,polar_crd(r, angle))
  let e1 = add(position, polar_crd(r, angle + 180deg))
  let s2 = add(position,polar_crd(r, angle + 90deg))
  let e2 = add(position,polar_crd(r, angle - 90deg))
  line(s1, e1)
  line(s2, e2)
}

#let Rot_group(g, angle, origin:(0,0,0))={

  rotate(angle, origin: origin)
  g
}

// example
(a)
#align(center)[
#canvas(length: 1cm,
{
  waveline((1,0),(2,0),node:6)
  circle((3,0),fill:gray)
  waveline((4,0),(5,0),node:6)
})
]

#let a =(1,2)
#let b = (2,2)

(b)
#align(center)[
#canvas(length: 1cm,
{
  dot(0,0)
  dot(-1,1)
  ppa_arc((0,0),(-1,1),120deg,re:1)
})]
#align(center)[
#canvas(length: 1cm,{

ph((1,2),(3,2),angle:120deg, node:16,vertex_show: true)
line((0,2),(4,2))
ph((2,2),(2,0),vertex_show: true,node: 16)
mark((1,-1),(1.5,-0.5),symbol:">",fill:black)
line((1,-1),(2,0))
line((2,0),(3,-1))
mark((2,0),(2.7,-0.7),symbol:">",fill:black)
}
)
]

(c)
#align(center)[
#canvas(length: 1cm, {
no_match_cir((2,0),(3,0))
}
)
]


#align(center)[
#canvas(length: 1cm, {
gluon((1,1),(1,2),node:8,vertex_show: false)
gluon((1.8,0.2),(1,1),node:8,vertex_show: false)

gluon((0.2,0.2),(1,1),node:8 )
}
)
]



#align(center)[
#canvas(length: 1cm,{
  fermion((0,0),(1,1), m_show: true,m:"p",re:-1)
  ph((1,1),(3,1),node:12, vertex_show: true)
  fermion((4,0),(3,1), m:"k")
  fermion((1,1),(0,2), m:$p'$,re:-1)
  fermion((3,1),(4,2), m_show: true,m:$k'$)
  content((2,0.7))[$gamma$]
  content((2,1.5))[$p'+p$]
  let c = (((-0.4,0),$e^-$),((-0.4,2),$e^+$),((4.4,0),$mu^+$),((4.4,2),$mu^-$))
  for i in range(4){
    content(c.at(i).at(0))[#c.at(i).at(1)]}
}) 
]
(d)


#align(center)[
#canvas(length: 1cm, {
  
  Dot((0,0))
  // let gr = arc_spilt((0,0),(1,2),angle:60deg, node: 16, re:1)
  // for i in range(gr.len()-1){
  //   elem_gluon(gr.at(i),gr.at(i+1))
  // }
  content((0,0))[O]
  Dot((3,0))
  Dot((3,3))
  gluon((3,0), (3,3), angle: 120deg, re: -1, node: 16)
  scalar((0,0),(2,0), angle: 180deg, form: 2, node:8, m:"k")
  scalar((2,0),(0,0), angle: 180deg, form:2)
  fermion_loop((2,4),(3,4), vertex_show: false)
  }
)
]


