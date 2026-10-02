---
layout: documentation-page
collectionName: Modes
title: Cheat Sheet
---

<style>
.table {
    width: fit-content;
    margin-left: auto;
    margin-right: auto;
    margin-bottom: 20px;
    border-style: solid;
    border-color: blue;
    border-radius: 25px;
    padding: 15px;
    text-align: center;
}
</style>

| Mode   | Lifetime                    | Allocation            |
| ------ | --------------------------- | --------------------- |
| `global` | MAY outlive its region      | MUST be on the heap   |
| `local`  | MUST NOT outlive its region | MAY be on the stack   |



<div style="display: flex">
<table style="border-collapse: collapse; width: 100%;">
<thead>
<tr>
<th style="width: 50px;"></th>
<th style="text-align: left; ">Past</th>
<th style="text-align: right;">Future</th>
</tr>
</thead>
<tbody>
<tr>
<td style="font-weight: bold; writing-mode: sideways-lr; text-align: center;">Region</td>
<td style="text-align: left;"> </td>
<td style="text-align: right;">Locality<br><code><em>global</em> < local</code></td>
</tr>
<tr>
<td style="font-weight: bold; writing-mode: sideways-lr; text-align: center;">Aliasing</td>
<td style="text-align: left;">Uniqueness<br><code>unique < <em>aliased</em></code></td>
<td style="text-align: right;">Affinity<br><code><em>many</em> < once</code></td>
</tr>
<tr>
<td style="font-weight: bold; writing-mode: sideways-lr; text-align: center;">Threads<br></td>
<td style="text-align: left;">Contention <br><code><em>uncontended</em> < shared < contended</code></td>
<td style="text-align: right;">Portability<br><code>portable < <em>nonportable</em></code></td>
</tr>
<tr>
<td style="font-weight: bold; writing-mode: sideways-lr; text-align: center;">Effects</td>
<td style="text-align: left;"></td>
<td style="text-align: right;">Yielding<br><code><em>unyielding</em> < yielding</code></td>
</tr>
<tr>
<td style="font-weight: bold; writing-mode: sideways-lr; text-align: center;">Purity</td>
<td style="text-align: left;">Visibility<br><code><em>read_write</em> < read < immutable</code></td>
<td style="text-align: right;">Statefulness<br><code>stateless < observing < <em>stateful</em></code></td>
</tr>
</tbody>
</table>
</div>



<div style="display: flex; justify-content: center;">
<table style="border-collapse: collapse; vertical-align: middle; text-align: center;">
<tr>
<td></td>
<td></td>
<td colspan="4" style="font-weight:bold;">Functions and function nesting</td>
</tr>
<tr>
<td style=" "></td>
<td style="font-weight: bold;">Aliasing<br>Threads<br>Purity</td>
<td>
  <code><em>many</em></code><br>
  <code>portable</code><br>
  <code>stateless</code></td>
<td>
  <br>
  <br>
  <code>observing</code></td>
<td>
  <code>once</code><br>
  <code><em>nonportable</em></code><br>
  <code><em>stateful</em></code></td>
</tr>
<tr>
<td rowspan="3" style="writing-mode: sideways-lr; font-weight: bold;">Mutable and mutable nesting</td>
<td>
  <code><em>aliased</em></code><br>
  <code>contended</code><br>
  <code>immutable</code></td>
<td>&check;</td>
<td>&check;</td>
<td>&check;</td>
</tr>
<tr>
<td>
  <br>
  <code>shared</code><br>
  <code>read </code></td>
<td></td>
<td>&check;</td>
<td>&check;</td>
</tr>
<tr>
<td>
  <code>unique</code><br>
  <code><em>uncontended</em></code><br>
  <code><em>read_write</em></code></td>
<td></td>
<td></td>
<td>&check;</td>
</tr>
</table>
</div>
